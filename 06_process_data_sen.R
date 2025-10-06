# Special educational needs in England #

# This publication contains information about pupils with special educational needs. 
# This information is derived from school sen returns, general hospital school sen 
# and school level annual school sen (SALSC, independent schools) returns made to the department in January each year. 
# The school sen contains pupil level data covering a wide range of information on the characteristics of schools and the pupils. 

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
dir_in <- file.path(dir_data, "special-educational-needs-in-england")

# determine year list (akin to other data sources)
years_list <- paste0(20, 10:24, 11:25)

id_cols <- c("time_period", "urn")

na_values <- c(":", "z", "NULL", "Unknown", "x", "..", "", ">", "^", ".")

# Get Information about Schools #

# read in establishment data
gias <- as.data.frame(fread(file.path(dir_data, "data_gias_estab.csv"), encoding = "UTF-8"))

# remove all establishments without an laestab (i.e., Children's centres, British schools overseas, Online providers)
gias <- gias[!is.na(gias$laestab), ]

# select relevant laestab and urn only and relevant columns
gias <- gias[!duplicated(gias), c("laestab", "urn", "establishmentname")]
names(gias) <- c("laestab", "urn_gias", "school")


# rename folders that currently only have one year included (data collected in Jan)
rename_folders <- F
if (rename_folders) {
  # save df with old and new folder names
  start <- 2010
  finish <- 2019
  tmp <- data.frame(old = c(start:finish))
  tmp$new <- paste0(paste0(tmp$old-1,"-", gsub(20, "",tmp$old)))
  # add dirs
  tmp$from <- file.path(dir_in, tmp$old)
  tmp$to <- file.path(dir_in, tmp$new)
  # rename
  file.rename(from = c(tmp$from), to = c(tmp$to))
}

# determine years of interest
start <- 2010 # no school level data for 2009-10
finish <- 2024

files <- list.files(path = dir_in,
                    pattern = "UD|ud|nderlying",
                    recursive = T,
                    full.names = T)
files <- files[!grepl("meta|Meta", files)]
files

# School sen data column lookup table based on metadata
column_lookup_sen <- tibble(
  standard_name = c(
    # Basic identifiers - handle structural changes
    "urn", "laestab", "la", "estab", "time_period",
    
    # Pupil roll counts - numbers only
    "num_pup_tot",
    
    # ===== SEN MEASURES =====
    # Old system pre-2014/15
    "num_pup_sen_a", 
    "num_pup_sen_ap", 
    "num_pup_sen_st", 
    "num_pup_sen_ns",
    
    # New system 2014-15plus
    "num_pup_sen_k", 
    "num_pup_sen_e",
    "num_pup_sen_se"
  ),
  
  # All possible variations for each standard name - in matching order
  variations = list(
    # Basic identifiers
    c("urn"),
    c("laestab"),
    c("la_code_old", "old_la_code"),
    c("estab"),
    c("time_period"),
    
    # Pupil roll counts
    c("pupils", "total_pupils"),
    
    # SEN measures - CORRECTED ORDER (old system) - numbers
    c("schoolaction", "school_action"),      # School Action (first in hierarchy)
    c("schoolactionplus", "school_action_plus"),   # School Action Plus (second in hierarchy)
    c("statements", "statement"),   # Statement (third in hierarchy)
    c("nostatements", "no_statements", "no_statement"),   # pupils with SEN without a statement
    
    # SEN measures - new system - numbers
    c("sen_support"),    # SEN support
    c("ehc_plan"),   # EHC plan
    c("statement/_ehc_plan", "statement_/_ehc_plan")   # EHC plan
    # EHC plans also includes statements of SEN. Statements were phased out from 2014 to 2018


  )
)

# Run the review of the column name lookup
review_lookup_mappings(lookup_table = column_lookup_sen)

# Create the reverse lookup for school sen data
reverse_lookup_sen <- create_reverse_lookup(column_lookup_sen)

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
  
  # read in sen data
  tmp <- fread(files[i], 
               fill = Inf,                    # whole file is read for detecting the number of columns.
               sep = ",",                     # Explicitly set separator
               quote = "\"",                  # Handle quoted fields
               strip.white = TRUE,            # Remove whitespace
               blank.lines.skip = TRUE,       # Skip empty lines
               encoding = "Latin-1",            # Handle encoding issues
               na.strings = na_values,        # IMPORTANT: Convert NA values during read
               header = TRUE,                 # use column names from file
               nrows = -1,                    # Read all rows
               showProgress = FALSE)   
  
  # replace %
  tmp <- tmp[, lapply(.SD, function(x) gsub("%", "", x))]
  
  # Convert to data.frame
  tmp <- as.data.frame(tmp)
  
  # change col names
  names(tmp) <- tolower(gsub("X...", "", names(tmp), fixed = T))
  names(tmp) <- gsub("x..", "perc.", names(tmp), fixed = T)
  names(tmp) <- gsub(".", "_", names(tmp), fixed = T)
  names(tmp) <- gsub(" ", "_", names(tmp), fixed = T)
  
  # print(names(tmp))
  
  # add year
  tmp$time_period <- time_period
  
  # **standardise COLUMN NAMES**
  tmp <- standardise_column_names(tmp, lookup = reverse_lookup_sen)
  
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
sen <- bind_rows(df_stan, .id = "academic_year")

# replace NA in LAESTAB where possible
sen$laestab <- ifelse(is.na(sen$laestab) & !is.na(sen$la) & !is.na(sen$estab),
                         as.numeric(paste0(sen$la, sen$estab)),
                         sen$laestab)

# check that no column is missing
column_lookup_sen$standard_name[! column_lookup_sen$standard_name %in% names(sen)]

# re-order columns
sen <- sen[, column_lookup_sen$standard_name]

# Exclude rows that fully consist of NA values
sen <- sen[apply(sen[, -1:-5], 1, function(row) !all(is.na(row))), ]

# # check for any letters
# sink("check.txt")
# print(apply(sen, 2, function(x) { unique(regmatches(x, gregexpr("[A-Za-z]+", x)))   }))
# sink()

# check urns and clean up data
sen <- cleanup_data(data_in = sen)


# Print summary of combined dataset
cat("\n--- COMBINED PUPILS DATASET SUMMARY ---\n")
cat("Total rows:", nrow(sen), "\n")
cat("Total columns:", ncol(sen), "\n")
cat("Academic years included:", length(unique(sen$time_period)), "\n")
cat("Years:", paste(sort(unique(sen$time_period)), collapse = ", "), "\n")

# Show year distribution
year_counts <- table(sen$time_period)
print(year_counts)

#### save data ####

gc()

# re-order columns
sen <- sen %>% 
  select(-c(la, estab)) %>%
  relocate(time_period, school, laestab, urn) %>%
  arrange(laestab, time_period)

# pupil SEN status: Plan or intervention


# compute number of NA obs per school per year
sen$na_count <- apply(sen[, grepl("num_pup_sen", names(sen))], 1, function(row) sum(is.na(row)))
sen$zero_count <- apply(sen[, grepl("num_pup_sen", names(sen))], 1, function(row) sum(row == 0, na.rm = T))
sen$missing_count <- sen$na_count + sen$zero_count

sen[, "num_pup_sen"] <- ifelse(sen$missing_count != sum(grepl("num_pup_sen", names(sen))),
                               rowSums(sen[, c("num_pup_sen_a", # School Action 2010/11 - 2013/14
                                               "num_pup_sen_ap", # School Action Plus 2010/11 - 2013/14
                                               "num_pup_sen_st", # Statement 2010/11 - 2013/14
                                               
                                               "num_pup_sen_k", # SEN support 2014/15 - 2024/25
                                               "num_pup_sen_se", # Statement or EHC 2014/15 - 2018/19
                                               "num_pup_sen_e" # EHC 2019/20 - 2024/25
                                               )], na.rm = T),
                               NA)

# sen[, "num_pup_sen_high"] <- ifelse(!is.na(sen$num_pup_sen),
#                                        rowSums(sen[, c("num_pup_sen_st", "num_pup_sen_e", "num_pup_sen_se")], na.rm = T),
#                                        NA)
# 
# sen[, "num_pup_sen_lower"] <- ifelse(!is.na(sen$num_pup_sen),
#                                         rowSums(sen[, c("num_pup_sen_a", "num_pup_sen_ap",
#                                                            "num_pup_sen_k")], na.rm = T),
#                                         NA)

sen <- sen[, !grepl("_count", names(sen))]

# # check total pupil number against SPT census #
# 
# # schools, pupils and characteristics
# spc <- fread(file.path(dir_data, "data_spc_pupils.csv")) # census data collected in January of academic year - Spring census
# spc <- as.data.frame(spt)
# names(spc)[names(spc) == "num_pup_total"] <- "num_spc_tot"
# 
# # performance table census
# spt <- fread(file.path(dir_data, "data_spt_census.csv"))
# spt <- as.data.frame(spt)
# names(spt)[names(spt) == "num_pup_tot"] <- "num_spt_tot"
# 
# # combine data from all sources
# 
# check <- merge(spc[, c("urn", "time_period", "num_spc_tot")], spt[, c("urn", "time_period", "num_spt_tot")], 
#                by = c("urn", "time_period"), all = T)
# 
# check <- merge(check, sen, by = c("urn", "time_period"), all = T)
# 
# # compare total number of pupils
# check$spt_sen <- check$num_spt_tot - check$num_pup_tot
# psych::describeBy(check$spt_sen, group = check$time_period, na.rm = T)
# 
# check %>% group_by(time_period) %>%
#   summarise(n = sum(spt_sen != 0, na.rm = T))
# 
# # 201011 - 201617: min -2 and max 2; SEN rounded but SPC not rounded
# 
# # rounding applied to nearest 5 in total pupil headcount data collected in 201011 / 201112 / 201213 in SPC
# # there is a difference between npuptot__spc & npuptot__sen only in the years 2013/14 - 2016/17
# #   data in  npuptot__sen is rounded to nearest 5, data in npuptot__spc is not rounded
# #   NOTE: data on total number of pupils is the same for SPC and SEN tables once SEN data is rounded to the nearest 5 for the years 2013/14 - 2016/17 (!)
# 
# sen <- merge(sen, spt[, c("urn", "time_period", "num_spt_tot")], by = c("urn", "time_period"), all.x = T)
# names(sen)[names(sen) == "num_pup_tot"] <- "num_sen_tot"
# 
# 
# col_tot <- "num_pup_tot"
# id_cols <- c("time_period", "laestab", "urn", "school")
# 
# # fix roundings
# fixed <- fix_roundings(var_rd = "num_sen_tot", var_nrd = "num_spt_tot",
#                        new_var = col_tot,
#                        identifier_columns = id_cols,
#                        col_to_filter = "time_period",
#                        filter = c(201011, 201112, 201213, 201314, 201415, 201516, 201617),
#                        rounding_factor = 5,
#                        data_in = sen)
# 
# # add to data.frame
# sen <- merge(fixed[, c(id_cols, col_tot)], sen, by = id_cols, all.y = T)
# sen$num_sen_tot <- NULL
# sen$num_spt_tot <- NULL

# write file

data.table::fwrite(sen, file = file.path(dir_data, "data_sen.csv"), row.names = F)
