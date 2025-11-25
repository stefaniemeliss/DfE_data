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
gias <- as.data.frame(fread(file.path(dir_data, "data_gias_estab.csv"), encoding = "UTF-8"))

# remove all establishments without an laestab (i.e., Children's centres, British schools overseas, Online providers)
gias <- gias[!is.na(gias$laestab), ]

# select relevant laestab and urn only and relevant columns
gias <- gias[!duplicated(gias), c("laestab", "urn", "establishmentname")]
names(gias) <- c("laestab", "urn_gias", "school")

# # SPC 2019 data
# spc <- fread(file.path(dir_data, "data_spc_pupils.csv")) # census data collected in January of academic year - Spring census
# spc <- as.data.frame(spc)
# 
# # get total number of pupils for 2019/20
# spc <- spc[spc$time_period == 201920, c("urn", "num_pup_total")]

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
    
    # Pupil roll counts
    "num_pup_tot", 
    "num_pup_girls", 
    "perc_pup_girls",
    "num_pup_boys", 
    "perc_pup_boys",
    
    # ===== SEN MEASURES =====
    # Old system pre-2014/15
    "num_pup_sen_a", 
    "perc_pup_sen_a",              # School Action (2010/11 - 2017/18)
    "num_pup_sen_ap", 
    "perc_pup_sen_ap",            # School Action Plus (2010/11 - 2017/18)
    "num_pup_sen_aps", 
    "perc_pup_sen_aps",  # School Action Plus or Statement (2010/11 - 2014/15)
    "num_pup_sen_st", 
    "perc_pup_sen_st",        # Statement (2010/11 - 2017/18)
    
    # New system 2014-15plus
    "num_pup_sen_k", 
    "perc_pup_sen_k",          # SEN Support (2014/15 - 2023/24)
    "num_pup_sen_e", 
    "perc_pup_sen_e",            # EHC Plan (2014/15 - 2023/24)
    
    # Language measures
    "num_pup_efl",
    "perc_pup_efl",
    "num_pup_eal",
    "perc_pup_eal",
    "num_pup_ufl",
    "perc_pup_ufl",

    # Language measures
    
    # Free School Meals measures
    "num_pup_fsm", # not in 2014/15 to 2017/18 and 2019/20 
    "perc_pup_fsm",  # only avail from 2010/11 to 2017/18
    "num_pup_tot_fsm_calc", # only avail from 2010/11 to 2013/14
    "num_pup_fsm6",
    "perc_pup_fsm6", # MISSING for 2019/20 --> FOI request
    "num_pup_tot_fsm6_calc" # only available 2021/22 onwards
    
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
    c("totpupsendn", "nor", "num_pup_total"),
    c("norb"),
    c("pnorb"),
    c("norg"),
    c("pnorg"),
    
    # ===== SEN MEASURES =====
    
    # Pre-2014 SEN system:
    #   - School Action (lowest level, reported as tsena)
    #   - School Action Plus (additional external support, reported as totsenap)
    #   - Statement of SEN (highest level, reported as totsenst)
    #
    # 2010/11 to 2013/14:
    #   tsensap: Number of pupils with SEN statement or on School Action Plus
    #            (groups the two highest levels, as was common in analysis at the time)
    #   totsenst: Total pupils with SEN statement
    #   totsenap: Total pupils at School Action Plus
    #   tsena: Number of pupils on roll with SEN on School Action
    #
    # Transition year:
    #   Statements were being replaced by EHC plans (from September 2014).
    #   School Action and School Action Plus were replaced by SEN Support.
    #
    # 2014/15:
    #   tsenelse: Number of SEN pupils with a statement or EHC plan (reflects the introduction of EHC plans)
    #   tsenelk: Number of eligible pupils with SEN support (reflects the new post-2014 support level)
    #
    # Bedding-in of the new system:
    #   Statements were being phased out, EHC plans phased in.
    #   SEN Support replaced School Action/Plus.
    #
    # 2015/16 to 2017/18:
    #   tsenelse: Number of SEN pupils with a statement or EHC plan (combines both statement and EHC plan as the highest level)
    #   tsenelk: Number of eligible pupils with SEN support (lower level of support)
    #
    # 2018/19 onwards:
    #   tsenelse: Number of SEN pupils with an EHC plan (statements fully replaced by EHC plans)
    #   tsenelk: Number of eligible pupils with SEN support (without an EHC plan)
    #
    # ===== Summary Table =====
    # Years              Highest level                Middle/Lower level            Grouped variables
    # 2010/11–2013/14    totsenst                     totsenap, tsena               tsensap (statement or SAP)
    # 2014/15            tsenelse                     tsenelk                       (no grouped variable; transition to new system)
    # 2015/16–2017/18    tsenelse                     tsenelk
    # 2018/19 onwards    tsenelse (EHC plan only)     tsenelk (SEN support)
    
    
    # SEN measures - CORRECTED ORDER (old system)
    c("tsena"),      # School Action (first in hierarchy)
    c("psena"),      # % School Action
    c("totsenap"),   # School Action Plus (second in hierarchy)
    c("ptotsenap"),  # % School Action Plus
    c("tsensap"),    # Combined: Statement or School Action Plus
    c("psensap"),    # % Statement or School Action Plus
    c("totsenst"),   # Statement (third in hierarchy)
    c("ptotsenst"),  # % Statement
    
    # SEN measures - new system
    c("tsenelk"),    # SEN support
    c("psenelk"),    # % SEN support
    c("tsenelse"),   # EHC plan
    c("psenelse"),   # % EHC plan
    
    # Language measures
    c("numengfl"),
    c("pnumengfl"),
    c("numeal"),
    c("pnumeal"),
    c("numuncfl"),
    c("pnumuncfl", "pnumunclf"),
    # c("totpupealdn"),
    
    # Language measures
    
    # Free School Meals measures
    c("numfsm"),
    c("pnumfsm"),
    c("totpupfsmdn"),
    c("numfsmever", "fsm6"),
    c("pnumfsmever"),
    c("norfsmever")
      )
)

# Run the review of the column name lookup
review_lookup_mappings(lookup_table = column_lookup_census)
write.csv(apply(column_lookup_census, 2, as.character), file = file.path(dir_misc, "meta_census.csv"), row.names = F)

# Create the reverse lookup for school census data
reverse_lookup_census <- create_reverse_lookup(column_lookup_census)

# Initialize empty list to store all processed datasets
df_all <- list()

# LOOP OVER ALL YEARS 
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
    
    # # add pupil count for 2019/20
    # tmp <- merge(tmp, spc, by = "urn", all.x = T)
    
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

# FSM data #

# create df with fsm relevant data only
check <- census[, grepl("urn|time|fsm|tot", names(census))]
check$num_pup_sen_tot <- NULL

# check data availability by year
check %>% group_by(time_period) %>%
  summarise(
    rows = sum(!is.na(urn_census)),
    
    num_pup_fsm = sum(is.na(num_pup_fsm)),
    perc_pup_fsm = sum(is.na(perc_pup_fsm)),
    num_pup_tot_fsm_calc = sum(is.na(num_pup_tot_fsm_calc)),    
    num_pup_fsm6 = sum(is.na(num_pup_fsm6)),
    perc_pup_fsm6 = sum(is.na(perc_pup_fsm6)),
    num_pup_tot_fsm6_calc = sum(is.na(num_pup_tot_fsm6_calc))
  )

# re-calculate perc of pupils eligible for FSM
check$perc_pup_fsm_RECALC <- ifelse(check$time_period %in% c(201011, 201112, 201213), round(check$num_pup_fsm / check$num_pup_tot_fsm_calc * 100, 1),
                                    round(check$num_pup_fsm / check$num_pup_tot * 100, 1))

# compute difference
check$diff_perc_pup_fsm <- check$perc_pup_fsm - check$perc_pup_fsm_RECALC

# check descriptives of diff
psych::describe(check$diff_perc_pup_fsm)


# SEN measures #

# compute number of NA obs per school per year
census$na_count <- apply(census[, grepl("num_pup_sen", names(census))], 1, function(row) sum(is.na(row)))
census$zero_count <- apply(census[, grepl("num_pup_sen", names(census))], 1, function(row) sum(row == 0, na.rm = T))
census$missing_count <- census$na_count + census$zero_count


# compute totals

# populate num_pup_sen_aps
# num_pup_sen_aps = num_pup_sen_ap + num_pup_sen_st
# pupils with statement or on school action plus
census$num_pup_sen_aps <- ifelse(census$time_period %in% c(201011, 201112, 201213, 201314) & is.na(census$num_pup_sen_aps), 
                                 rowSums(census[, c("num_pup_sen_ap", "num_pup_sen_st")], na.rm = T), census$num_pup_sen_aps)

# deduct pupils on school action plus where possible
census$num_pup_sen_ap <- ifelse(census$time_period %in% c(201011, 201213, 201314) & is.na(census$num_pup_sen_ap) & !is.na(census$num_pup_sen_aps) & !is.na(census$num_pup_sen_st), 
                                 census$num_pup_sen_aps - census$num_pup_sen_st, census$num_pup_sen_ap)


# deduct pupils with statement where possible
census$num_pup_sen_st <- ifelse(census$time_period %in% c(201011, 201213, 201314) & is.na(census$num_pup_sen_st) & !is.na(census$num_pup_sen_aps) & !is.na(census$num_pup_sen_ap), 
                                 census$num_pup_sen_aps - census$num_pup_sen_ap, census$num_pup_sen_st)



# Compute total of pupils with SEN
census[, "num_pup_sen_tot"] <- ifelse(census$missing_count != sum(grepl("num_pup_sen", names(census))),
                                      rowSums(census[, c("num_pup_sen_a", # School Action 2010/11 - 2013/14
                                                         "num_pup_sen_aps", # School Action Plus or Statement 2010/11 - 2013/14
                                                         "num_pup_sen_k", # SEN support 2014/15 - 2024/25
                                                         "num_pup_sen_e" # EHC 2014/15 - 2024/25 (until 2018/19, also included Statement)
                                                         )], na.rm = T),
                                      NA)

# Compute percentage of pupils with SEN
census[, "perc_pup_sen_tot"] <- ifelse(census$missing_count != sum(grepl("perc_pup_sen", names(census))),
                                      rowSums(census[, c("perc_pup_sen_a", # School Action 2010/11 - 2013/14
                                                         "perc_pup_sen_aps", # School Action Plus or Statement 2010/11 - 2013/14
                                                         "perc_pup_sen_k", # SEN support 2014/15 - 2024/25
                                                         "perc_pup_sen_e" # EHC 2014/15 - 2024/25 (until 2018/19, also included Statement)
                                                         )], na.rm = T),
                                      NA)
# override all values above 100 with 100
census[, "perc_pup_sen_tot"] <- ifelse(census[, "perc_pup_sen_tot"] > 100, 100, census[, "perc_pup_sen_tot"])

# delete count columns
census[, grepl("_count", names(census))] <- NULL


# census[, "num_pup_sen_high"] <- ifelse(!is.na(census$num_pup_sen_tot),
#                                rowSums(census[, c("num_pup_sen_st", "num_pup_sen_e", "num_pup_sen_aps")], na.rm = T),
#                                NA)
# note: in 2011/12, there is only data on school action and statement or school action plus

# census[, "num_pup_sen_lower"] <- ifelse(!is.na(census$num_pup_sen_tot),
#                                       rowSums(census[, c("num_pup_sen_a", "num_pup_sen_ap",
#                                                          "num_pup_sen_k")], na.rm = T),
#                                       NA)

# add NAs for 2019/20
census[census$time_period == 201920, grepl("pup_sen", names(census))] <- NA

# re-order and drop columns
census <- census %>% 
  relocate(num_pup_sen_tot, perc_pup_sen_tot, .before = num_pup_sen_a) %>%
  arrange(laestab, time_period)

# save file
data.table::fwrite(census, file = file.path(dir_data, "data_spt_census.csv"), row.names = F)
