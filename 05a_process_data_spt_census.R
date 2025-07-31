# school performance tables - census data #

# Data source: the DfE’s January school census for 2024.

options(scipen = 999)
# empty work space
rm(list = ls())
gc()

# load libraries
library(dplyr)
library(purrr)
library(rlang)
library(data.table)

# create function to source code
source_code <- function(root_dir_name = "code", target_repo = "helper_functions", branch = "main", file_name = "file.R") {
  
  # construct URL
  git_url <- paste0("https://raw.githubusercontent.com/stefaniemeliss/", target_repo, "/", branch, "/", file_name)
  
  # attempt to download from github
  tempp_file <- tempfile(fileext = ".R")
  message <- curl::curl_download(git_url, tempp_file, quiet = F)
  
  if(!grepl("Error", message)) {
    
    # if successful, source file
    source(tempp_file)
    remove(tempp_file)
    
  } else { # load local copy of file
    
    # Get the current working directory
    current_dir <- getwd()
    
    # Split the current directory into its components
    dir_components <- strsplit(current_dir, "/")[[1]]
    
    # Identify the root directory dynamically based on the provided root directory name
    root_index <- which(dir_components == root_dir_name)
    if (length(root_index) == 0) {
      stop(paste("Root directory", root_dir_name, "not found in the current path"))
    }
    root_dir <- do.call(file.path, as.list(dir_components[1:root_index]))
    
    # Identify the subdirectory one level below the root and construct its absolute path
    project_repo <- dir_components[root_index + 1]
    dir <- file.path(root_dir, project_repo)
    
    if (target_repo != project_repo) {
      dir <- gsub(project_repo, target_repo, dir) 
    }
    
    # Construct the full file path
    file_path <- file.path(dir, file_name)
    
    # Print the directory and file path for debugging
    print(paste("Directory:", dir))
    print(paste("File path:", file_path))
    
    # Source the file into the parent frame
    source(file_path, local = parent.frame())
  }
}

# source functions
source_code(target_repo = "DfE_data", file_name = "functions.R")

# define directories
dir <- getwd()
dir_data <- file.path(dir, "data")
dir_misc <- file.path(dir, "misc")
dir_in <- file.path(dir_data, "performance-tables")

# script variable definition #

# determine year list (akin to other data sources)
years_list <- paste0(20, 10:23, 11:24)
lookup <- data.frame(time_period = as.numeric(years_list),
                     academic_year = as.numeric(substr(years_list, 1, 4)))

# determine years of interest
start <- 2010
finish <- 2023

# Define NA values first
na_values <- c("SUPP", "NP", "", "NE", "NA", "LOWCOV", "SP", "RE", "NEW", "UNAVAIL", "NAT", "PRI", "SEC", "SPE")

# Get Information about Schools #

# read in establishment data
gias <- as.data.frame(fread(file.path(dir_data, "data_gias_search.csv"), encoding = "UTF-8"))

# remove all establishments without an laestab (i.e., Children's centres, British schools overseas, Online providers)
gias <- gias[!is.na(gias$laestab), ]

# select relevant laestab and urn only and relevant columns
gias <- gias[!duplicated(gias), c("laestab", "urn", "establishmentname")]
names(gias) <- c("laestab", "urn_gias", "school")


# Best Practice for Matching Inconsistent Column Names Across Years

## 1. Create a Column Name Correspondence Table
## 2. Automated Renaming Using dplyr and purrr
## 3. Use bind_rows() for Efficient Combining
## 4. Clean id information

# Process meta data # 
for (i in seq_along(start:finish)) {
  
  year = c(start:finish)[i]  
  # skip covid years
  if(year == 2019) next
  
  # determine academic year
  academic_year <- paste0(year,"-", year+1)
  time_period <- as.numeric(gsub("-20", "", academic_year))
  
  # determine folder for academic year
  dir_year <- file.path(dir_in, academic_year)
  
  # get meta data
  tmp_meta <- read.csv(file = file.path(dir_year, paste0(academic_year, "_census_meta.csv")))
  names(tmp_meta) <- gsub("X...", "", tolower(names(tmp_meta)), fixed = T)
  names(tmp_meta)[names(tmp_meta) == "field.reference"] <- "variable"
  names(tmp_meta)[names(tmp_meta) == "field.name"] <- "label"
  tmp_meta <- tmp_meta[, c("variable", "label")]
  tmp_meta$time_period <- time_period
  tmp_meta$variable <- tolower(tmp_meta$variable)
  
  # combine across years
  if (year == start) {
    # census <- tmp
    meta <- tmp_meta
  } else {
    # census <- rbind.all.columns(census, tmp)
    meta <- rbind.all.columns(meta, tmp_meta)
  }
  
}
# save meta data to xlsx
xlsx_file <- file.path(dir_misc, "meta_spt.xlsx")
sheet_name <- "census"

if (file.exists(xlsx_file)) {
  wb <- openxlsx::loadWorkbook(xlsx_file) # Load the workbook if it exists
  if (sheet_name %in% names(wb)) openxlsx::removeWorksheet(wb, sheet_name) # Remove the sheet if it already exists
} else {
  wb <- openxlsx::createWorkbook() # Create a new workbook if it does not exist
}
openxlsx::addWorksheet(wb, sheet_name) # Add the new sheet with your data
openxlsx::writeData(wb, sheet_name, meta) # Write object to worksheet
openxlsx::saveWorkbook(wb, xlsx_file, overwrite = TRUE) # Save the workbook (overwriting if necessary)



# School Census data column lookup table based on metadata
column_lookup_census <- tibble(
  standard_name = c(
    # Basic identifiers - handle structural changes
    "urn", "laestab", "la", "estab", "time_period",
    
    # Pupil roll counts - numbers only
    "num_pup_tot", "num_pup_girls", "num_pup_boys", 

    # # Gender percentages
    # "perc_pup_girls", "perc_pup_boys",
    
    # ===== SEN MEASURES =====
    # Old system pre-2014/15
    "num_pup_sen_a", 
    "num_pup_sen_ap", 
    "num_pup_sen_aps", 
    "num_pup_sen_st", 
    
    # New system 2014-15plus
    "num_pup_sen_k", 
    "num_pup_sen_e", 
    
    "perc_pup_sen_a",              # School Action (2010/11 - 2017/18)
    "perc_pup_sen_ap",            # School Action Plus (2010/11 - 2017/18)
    "perc_pup_sen_aps",  # School Action Plus or Statement (2010/11 - 2014/15)
    "perc_pup_sen_st",        # Statement (2010/11 - 2017/18)
    
    "perc_pup_sen_k",          # SEN Support (2014/15 - 2023/24)
    "perc_pup_sen_e",            # EHC Plan (2014/15 - 2023/24)
    
    # Language measures - numbers
    "num_pup_eal",
    "num_pup_efl",
    "num_pup_ufl",
    # "num_pup_used_for_eal_calculation", 
    
    # # Language measures - percentages
    # "perc_pup_eal",
    # "perc_pup_efl", 
    # "perc_pup_ufl",
    
    # Free School Meals measures - numbers
    "num_pup_fsm",
    "num_pup_fsm6",
    "num_pup_used_for_fsm_calculation",
    "num_pup_used_for_fsm6_calculation",
    
    # Free School Meals measures - percentages
    "perc_pup_fsm",
    "perc_pup_fsm6"
  ),
  
  # All possible variations for each standard name - in matching order
  variations = list(
    # Basic identifiers
    c("urn"),
    c("laestab"),
    c("la"),
    c("estab"),
    c("time_period"),
    
    # Pupil roll counts
    c("totpupsendn", "nor"),
    c("norg"),
    c("norb"),

    # # Gender percentages
    # c("pnorg"),
    # c("pnorb"),
    
    # SEN measures - CORRECTED ORDER (old system) - numbers
    c("tsena"),      # School Action (first in hierarchy)
    c("totsenap"),   # School Action Plus (second in hierarchy)
    c("tsensap"),    # Combined: Statement or School Action Plus
    c("totsenst"),   # Statement (third in hierarchy)
    
    # SEN measures - new system - numbers
    c("tsenelk"),    # SEN support
    c("tsenelse"),   # EHC plan
    
    # SEN measures - CORRECTED ORDER (old system) - percentages
    c("psena"),      # % School Action
    c("ptotsenap"),  # % School Action Plus
    c("psensap"),    # % Statement or School Action Plus
    c("ptotsenst"),  # % Statement
    
    # SEN measures - new system - percentages
    c("psenelk"),    # % SEN support
    c("psenelse"),   # % EHC plan
    
    # Language measures - numbers
    c("numeal"),
    c("numengfl"),
    c("numuncfl"),
    # c("totpupealdn"),
    
    # # Language measures - percentages
    # c("pnumeal"),
    # c("pnumengfl"),
    # c("pnumuncfl", "pnumunclf"),
    
    # Free School Meals measures - numbers
    c("numfsm"),
    c("numfsmever", "fsm6"),
    c("totpupfsmdn"),
    c("norfsmever"),
    
    # Free School Meals measures - percentages
    c("pnumfsm"),
    c("pnumfsmever")
  )
)

# Run the review of the column name lookup
review_lookup_mappings(lookup_table = column_lookup_census)

# Create the reverse lookup for school census data
reverse_lookup_census <- create_reverse_lookup(column_lookup_census)

# Initialize empty list to store all processed datasets
df_all <- list()

# LOOP OVER ALL YEARS 
sink("check.txt")
for (i in seq_along(start:finish)) {
  
  year = c(start:finish)[i]  

  # determine academic year
  academic_year <- paste0(year,"-", year+1)
  time_period <- as.numeric(gsub("-20", "", academic_year))
  
  # determine folder for academic year
  dir_year <- file.path(dir_in, academic_year)
  
  if (year == 2019) {
    
    # read in census data
    file = file.path(dir_misc, "FOI_2025-0008906_SMeliss_FSM6_Final.xlsx")
    tmp <- xlsx::read.xlsx(file = file, sheetIndex = 1, startRow = 3)
    
  } else {
    
    # read in census data
    file = file.path(dir_year, paste0(academic_year, "_england_census.csv"))
    tmp <- fread(file, 
                 fill = Inf,                    # whole file is read for detecting the number of columns.
                 sep = ",",                     # Explicitly set separator
                 quote = "\"",                  # Handle quoted fields
                 strip.white = TRUE,            # Remove whitespace
                 blank.lines.skip = TRUE,       # Skip empty lines
                 encoding = "UTF-8",            # Handle encoding issues
                 na.strings = na_values,        # IMPORTANT: Convert NA values during read
                 header = TRUE,                 # use column names from file
                 nrows = -1,                    # Read all rows
                 showProgress = FALSE)   
    
    # replace %
    tmp <- tmp[, lapply(.SD, function(x) gsub("%", "", x))]
    
    # Convert to data.frame
    tmp <- as.data.frame(tmp)
    
  }
  
  # change col names
  names(tmp) <- tolower(gsub("X...", "", names(tmp), fixed = T))
  names(tmp) <- gsub("x..", "perc.", names(tmp), fixed = T)
  names(tmp) <- gsub(".", "_", names(tmp), fixed = T)
  names(tmp) <- gsub(" ", "_", names(tmp), fixed = T)
  
  # Exclude rows that fully consist of NA values
  tmp <- tmp[apply(tmp, 1, function(row) !all(is.na(row))), ]
  
  # add year
  tmp$time_period <- time_period
  
  # **standardise COLUMN NAMES**
  tmp <- standardise_column_names(tmp, lookup = reverse_lookup_census)
  
  # subset to exclude any non-school level data
  tmp <- tmp %>%
    filter(! toupper(urn) %in% c("NAT"))
  
  # make all columns numeric
  tmp <- apply(tmp, 2, as.numeric) %>% as.data.frame()
  
  # Store processed dataset in list with academic year (encoded as time_period) as name
  df_all[[academic_year]] <- tmp
  
  # Print progress
  cat(paste("Processed", academic_year, "- Rows:", nrow(tmp), "Columns:", ncol(tmp), "\n"))
  
  rm(tmp)
  
}
sink()

# **BIND ALL DATASETS TOGETHER**
# Get all unique column names across all datasets
cols <- unique(unlist(lapply(df_all, names)))

# Ensure all datasets have the same columns (fill missing with NA)
df_stan <- lapply(df_all, function(df) {
  missing_cols <- setdiff(cols, names(df))
  for (col in missing_cols) {
    df[[col]] <- NA
  }
  return(df[cols])  # Reorder columns consistently
})

# Combine all datasets using bind_rows
census <- bind_rows(df_stan, .id = "academic_year")

# replace NA in LAESTAB where possible
census$laestab <- ifelse(is.na(census$laestab) & !is.na(census$la) & !is.na(census$estab),
                            as.numeric(paste0(census$la, census$estab)),
                            census$laestab)

# check that no column is missing
column_lookup_census$standard_name[! column_lookup_census$standard_name %in% names(census)]

# re-order columns
census <- census[, column_lookup_census$standard_name]

# Exclude rows that fully consist of NA values
census <- census[apply(census[, -1:-5], 1, function(row) !all(is.na(row))), ]

# # check for any letters
# sink("check.txt")
# print(apply(census, 2, function(x) { unique(regmatches(x, gregexpr("[A-Za-z]+", x)))   }))
# sink()

# check urns and clean up data
census <- cleanup_data(data_in = census)


# Print summary of combined dataset
cat("\n--- COMBINED PUPILS DATASET SUMMARY ---\n")
cat("Total rows:", nrow(census), "\n")
cat("Total columns:", ncol(census), "\n")
cat("Academic years included:", length(unique(census$time_period)), "\n")
cat("Years:", paste(sort(unique(census$time_period)), collapse = ", "), "\n")

# Show year distribution
year_counts <- table(census$time_period)
print(year_counts)

#### save data ####

gc()

# re-order columns
census <- census %>% 
  select(-c(la, estab)) %>%
  relocate(time_period, school, laestab, urn) %>%
  arrange(laestab, time_period)

# fill in number of boys/girls for all single sex schools
census$num_pup_boys <- ifelse(is.na(census$num_pup_boys) & !is.na(census$num_pup_girls), 0, census$num_pup_boys)
census$num_pup_girls <- ifelse(is.na(census$num_pup_girls) & !is.na(census$num_pup_boys), 0, census$num_pup_girls)

# Re-compute percentages

denominator <- "num_pup_tot"

# # Gender percentages 
# numerator <- "num_pup_girls"
# quotient <- "perc_pup_girls" 
# census[, quotient] <- census[, numerator] / census[, denominator] * 100
# 
# numerator <- "num_pup_boys"
# quotient <- "perc_pup_boys" 
# census[, quotient] <- census[, numerator] / census[, denominator] * 100

# SEN measures

# compute totals
census$num_pup_sen_tot <- rowSums(census[, c("num_pup_sen_a", "num_pup_sen_ap", "num_pup_sen_st", "num_pup_sen_k", "num_pup_sen_e")], na.rm = T)
census$num_pup_sen_high <- rowSums(census[, c("num_pup_sen_st", "num_pup_sen_e")], na.rm = T)
census$num_pup_sen_lower <- rowSums(census[, c("num_pup_sen_a", "num_pup_sen_ap", "num_pup_sen_k")], na.rm = T)

# add NAs for 2019/20
levels <- c("tot", "high", "lower")
census[census$time_period == 201920, paste0("num_pup_sen_", levels)] <- NA


# compute percentages
for (i in 1:length(levels)) {
  numerator <- paste0("num_pup_sen_", levels[i])
  quotient <- paste0("perc_pup_sen_", levels[i])
  census[, quotient] <- census[, numerator] / census[, denominator] * 100
}

# re-order and drop columns
census <- census %>% 
  relocate(any_of(c(paste0("num_pup_sen_", levels), paste0("perc_pup_sen_", levels))), .after = num_pup_boys) %>%
  arrange(laestab, time_period)

# delete more granular SEN data
levels <- c("a", "ap", "st", "k", "e", "aps")
census[, paste0("num_pup_sen_", levels)] <- NULL
census[, paste0("perc_pup_sen_", levels)] <- NULL

 
# # Language measures - percentages
# 
# # Number of pupils with English as first language not included in 2011-12, re-compute using num_pup_used_for_eal_calculation variable where possible
# census$num_pup_efl <- ifelse(census$time_period == 201112, (census$num_pup_used_for_eal_calculation - census$num_pup_eal - census$num_pup_ufl), census$num_pup_efl)
# # Number of pupils used for EAL computations mostly missing, re-compute as sum of EFL, EAL and unclassified
# census$num_pup_used_for_eal_calculation <- ifelse(is.na(census$num_pup_used_for_eal_calculation), (census$num_pup_efl + census$num_pup_eal + census$num_pup_ufl), census$num_pup_used_for_eal_calculation)


# # in theory, this number *could* be used to re-compute the percentages for language measures
# denominator <- "num_pup_used_for_eal_calculation"
# 
# numerator <- "num_pup_eal"
# quotient <- "perc_pup_eal" 
# census[, quotient] <- census[, numerator] / census[, denominator] * 100
# 
# numerator <- "num_pup_efl"
# quotient <- "perc_pup_efl" 
# census[, quotient] <- census[, numerator] / census[, denominator] * 100
# 
# numerator <- "num_pup_ufl"
# quotient <- "perc_pup_ufl" 
# census[, quotient] <- census[, numerator] / census[, denominator] * 100

# Free School Meals measures - percentages

# # in theory, the percentages could be recomputed as shown below but there is too much data missing
# 
# # FSM 
# denominator <- "num_pup_used_for_fsm_calculation"
# 
# numerator <- "num_pup_fsm_eligible"
# quotient <- "perc_pup_fsm_eligible" 
# census[, quotient] <- census[, numerator] / census[, denominator] * 100
# 
# # FSM EVER
# denominator <- "num_pup_used_for_fsm_ever_calculation"
# 
# numerator <- "num_pup_fsm_ever_6_years"
# quotient <- "perc_pup_fsm_ever_6_years" 
# census[, quotient] <- census[, numerator] / census[, denominator] * 100

# save file
data.table::fwrite(census, file = file.path(dir_data, "data_spt_census.csv"), row.names = F)
