# Schools, pupils and their characteristics #

# This release contains the latest statistics on school and pupil numbers and their characteristics, 
# including age, gender, free school meals (FSM) eligibility, English as an additional language, ethnicity, school characteristics, class sizes.
# The publication combines information from the school census, school level annual school census, 
# general hospital school census and alternative provision census.

# The most recently published data is from the school census which took place on 18th January 2024 (Spring census)

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
dir_in <- file.path(dir_data, "school-pupils-and-their-characteristics")

# script variable definition #

# determine year list (akin to other data sources)
years_list <- paste0(20, 10:23, 11:24)
lookup <- data.frame(time_period = as.numeric(years_list),
                     academic_year = as.numeric(substr(years_list, 1, 4)))

# determine years of interest
start <- 2010
finish <- 2024

# Define NA values first
na_values <- c(":", "z", "NULL", "Unknown", "x", "..", "", ">", "^")


# rename folders that currently only have one year included (data collected in Jan)
rename_folders <- F
if (rename_folders) {
  # save df with old and new folder names
  start <- 2010
  finish <- 2019
  # data collection takes place in Jan of the year
  # e.g., data from 2014 is from 2013/14
  tmp <- data.frame(old = c(start:finish)) 
  tmp$new <- paste0(paste0(tmp$old-1,"-", gsub(20, "",tmp$old)))
  # add dirs
  tmp$from <- file.path(dir_in, tmp$old)
  tmp$to <- file.path(dir_in, tmp$new)
  # rename
  file.rename(from = c(tmp$from), to = c(tmp$to))
}

# Get Information about Schools #

# read in establishment data
gias <- as.data.frame(fread(file.path(dir_data, "data_gias_estab.csv"), encoding = "UTF-8"))

# remove all establishments without an laestab (i.e., Children's centres, British schools overseas, Online providers)
gias <- gias[!is.na(gias$laestab), ]

# select relevant laestab and urn only and relevant columns
gias <- gias[!duplicated(gias), c("laestab", "urn", "establishmentname")]
names(gias) <- c("laestab", "urn_gias", "school")



#### combine data ####

# Best Practice for Matching Inconsistent Column Names Across Years

## 1. Create a Column Name Correspondence Table
## 2. Automated Renaming Using dplyr and purrr
## 3. Use bind_rows() for Efficient Combining
## 4. Clean id information

# pupils on roll #

# list data files
files_pupils <- list.files(path = dir_in,
                           pattern = "school_level|School_level_school|Schools_Pupils_UD|pupil_characteristics_UD",
                           recursive = T,
                           full.names = T)
files_pupils <- files_pupils[!grepl("Meta|meta|ncyear|class|census|TEMPLATE", files_pupils)]
files_pupils


# Create column name correspondence table for pupil data
column_lookup_pupils <- tibble(
  standard_name = c(
    # Basic identifiers (specified ones only)
    "time_period", "urn", "laestab", "old_la_code", "estab", "school_name", 
    
    # Pupil counts (numbers only, no percentages)
    "fte_pupils", "num_pup_total", "num_pup_boys", "num_pup_girls", 
    "boarders_total", "boarders_boys", "boarders_girls", 
    
    # Key Stage and Year Group Measures (appears 2018-2025)
    "num_pup_ey",
    "num_pup_nurs", 
    "num_pup_recep",
    "num_pup_ks1",
    "num_pup_ks2",
    "num_pup_ks3",
    "num_pup_ks4", 
    "num_pup_ks5",
    
    # Free School Meals (numbers only, no percentages)  
    "num_pup_fsm",
    "perc_pup_fsm",
    "num_pup_tot_fsm_calc", # only avail from 2010/11 to 2012/13
    "num_pup_fsm_performance_tables",
    "perc_pup_fsm_performance_tables",
    "num_pup_tot_fsm_calc_performance_tables",
    
    # Language (numbers only)
    "num_pup_efl",
    "num_pup_eal", 
    "num_pup_ufl",
    
    # Focused Ethnicity - White British only
    "num_pup_ethn_white_british",
    
    # Focused Ethnicity - Mixed groups (all mixed heritage categories)
    "num_pup_ethn_mixed_white_and_black_caribbean", 
    "num_pup_ethn_mixed_white_and_black_african",
    "num_pup_ethn_mixed_white_and_asian",
    "num_pup_ethn_mixed_other",
    
    # Focused Ethnicity - Black groups (combined mapping)
    "num_pup_ethn_black_caribbean",
    "num_pup_ethn_black_african",
    "num_pup_ethn_black_other",
    
    # Focused Ethnicity - Asian groups (selected)
    "num_pup_ethn_asian_indian",
    "num_pup_ethn_asian_pakistani", 
    "num_pup_ethn_asian_bangladeshi",
    "num_pup_ethn_asian_chinese",
    "num_pup_ethn_asian_other",
    
    # Ethnicity - Unclassified
    "num_pup_ethn_unclassified"
    
    
  ),
  
  # All possible variations for each standard name
  variations = list(
    # Basic identifiers
    c("time_period"),
    c("urn"),
    c("laestab"),
    c("old_la_code"),
    c("estab", "estab_number"),
    c("school_name"),
    
    # Pupil counts - handling gender terminology changes and rounding variations
    c("fte_pupils", "fte_pupils_(unrounded)"),
    c("headcount_of_pupils", "headcount_of_pupils_(unrounded)"),
    c("headcount_total_boys", "headcount_total_male", "headcount_total_boys_(rounded)"),
    c("headcount_total_girls", "headcount_total_female", "headcount_total_girls_(rounded)"),
    c("total_boarders"),
    c("boy_boarders", "male_boarders"),
    c("girl_boarders", "female_boarders"),
    
    # Key Stage and Year Group Measures (NEW ADDITIONS - appear 2018-2025)
    c("number_of_early_year_pupils_(years_e1_and_e2)", "number_of_early_year_pupils__years_e1_and_e2_"),
    c("number_of_nursery_pupils_(years_n1_and_n2)", "number_of_nursery_pupils__years_n1_and_n2_"), # Number of nursery pupils (years N1 and N2)
    c("number_of_reception_pupils_(year_r)", "number_of_reception_pupils__year_r_"),
    c("number_of_key_stage_1_pupils_(years_1_and_2)", "number_of_key_stage_1_pupils__years_1_and_2_"),
    c("number_of_key_stage_2_pupils_(years_3_to_6)", "number_of_key_stage_2_pupils__years_3_to_6_"),
    c("number_of_key_stage_3_pupils_(years_7_to_9)", "number_of_key_stage_3_pupils__years_7_to_9_"), 
    c("number_of_key_stage_4_pupils_(years_10_and_11)", "number_of_key_stage_4_pupils__years_10_and_11_"),
    c("number_of_key_stage_5_pupils_(years_12_to_14)", "number_of_key_stage_5_pupils__years_12_to_14_"),

    # Free School Meals - multiple variations (numbers only)
    c("number_of_pupils_known_to_be_eligible_for_free_school_meals", 
      "number_of_pupils_known_to_be_eligible_for_and_claiming_free_school_meals"),
    c("%_of_pupils_known_to_be_eligible_for_free_school_meals", 
      "%_of_pupils_known_to_be_eligible_for_and_claiming_free_school_meals"),
    c("number_of_pupils_(used_for_fsm_calculation)"),
    c("number_of_pupils_known_to_be_eligible_for_free_school_meals_(performance_tables)"),
    c("%_of_pupils_known_to_be_eligible_for_free_school_meals_(performance_tables)"),
    c("number_of_pupils_(used_for_fsm_calculation_in_performance_tables)"),
    
    # Language (numbers only)
    c("number_of_pupils_whose_first_language_is_known_or_believed_to_be_english"),
    c("number_of_pupils_whose_first_language_is_known_or_believed_to_be_other_than_english"),
    c("number_of_pupils_whose_first_language_is_unclassified"),
    
    # Focused ethnicity fields (numbers only - no percentages)
    # White British
    c("number_of_pupils_classified_as_white_british_ethnic_origin"),
    
    # Mixed ethnicity groups - all variations
    c("number_of_pupils_classified_as_white_and_black_caribbean_ethnic_origin"),
    c("number_of_pupils_classified_as_white_and_black_african_ethnic_origin"),
    c("number_of_pupils_classified_as_white_and_asian_ethnic_origin"),
    c("number_of_pupils_classified_as_any_other_mixed_background_ethnic_origin"),
    
    # Black groups - mapping variations to standardized names
    c("number_of_pupils_classified_as_caribbean_ethnic_origin", 
      "number_of_pupils_classified_as_black_caribbean_ethnic_origin"),
    c("number_of_pupils_classified_as_african_ethnic_origin", 
      "number_of_pupils_classified_as_black_african_ethnic_origin"),
    c("number_of_pupils_classified_as_any_other_black_background_ethnic_origin"),
    
    # Asian groups - selected ones only
    c("number_of_pupils_classified_as_indian_ethnic_origin"),
    c("number_of_pupils_classified_as_pakistani_ethnic_origin"),
    c("number_of_pupils_classified_as_bangladeshi_ethnic_origin"),
    c("number_of_pupils_classified_as_chinese_ethnic_origin"),
    c("number_of_pupils_classified_as_any_other_asian_background_ethnic_origin"),
    
    # Unclassified
    c("number_of_pupils_unclassified")
  )
  
)

# Run the review of the column name lookup
review_lookup_mappings(lookup_table = column_lookup_pupils)
write.csv(apply(column_lookup_pupils, 2, as.character), file = file.path(dir_misc, "meta_spc_pupils.csv"), row.names = F)

# Create the reverse lookup
reverse_lookup_pupils <- create_reverse_lookup(column_lookup_pupils)

# Initialize empty list to store all processed datasets
ud_all_p <- list()

# LOOP OVER ALL YEARS 
for (i in seq_along(start:finish)) {
  year = c(start:finish)[i]
  
  # determine academic year
  academic_year <- paste0(year,"-", year+1)
  time_period <- as.numeric(gsub("-20", "", academic_year))
  
  # read in data: Use fread with more robust settings
  tmp_p <- fread(files_pupils[i], 
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
  
  # Convert to data.frame if needed for your pipeline
  tmp_p <- as.data.frame(tmp_p)
  
  # change col names
  names(tmp_p) <- tolower(gsub("X...", "", names(tmp_p), fixed = T))
  names(tmp_p) <- gsub("x..", "perc.", names(tmp_p), fixed = T)
  names(tmp_p) <- gsub(".", "_", names(tmp_p), fixed = T)
  names(tmp_p) <- gsub(" ", "_", names(tmp_p), fixed = T)
  
  # add time period
  tmp_p$time_period <- time_period
  
  # **standardise COLUMN NAMES**
  tmp_p <- standardise_column_names(tmp_p, lookup = reverse_lookup_pupils)
  
  # subset to exclude any non-school level data
  tmp_p <- tmp_p %>% filter(urn != "" & urn != 0)
  
  # remove school that was registered twice
  tmp_p <- tmp_p[!(tmp_p$urn == 143840 & tmp_p$estab == 6008), ]

  # Store processed dataset in list with academic year (encoded as time_period) as name
  ud_all_p[[academic_year]] <- tmp_p
  
  # Print progress
  cat(paste("Processed", academic_year, "- Rows:", nrow(tmp_p), "Columns:", ncol(tmp_p), "\n"))
  
  rm(tmp_p)
}

# **BIND ALL DATASETS TOGETHER**
# Get all unique column names across all datasets
cols_p <- unique(unlist(lapply(ud_all_p, names)))

# Ensure all datasets have the same columns (fill missing with NA)
ud_stan_p <- lapply(ud_all_p, function(df) {
  missing_cols <- setdiff(cols_p, names(df))
  for (col in missing_cols) {
    df[[col]] <- NA
  }
  return(df[cols_p])  # Reorder columns consistently
})

# Combine all datasets using bind_rows
ud_pupils <- bind_rows(ud_stan_p, .id = "academic_year")

# check that no column is missing
column_lookup_pupils$standard_name[! column_lookup_pupils$standard_name %in% names(ud_pupils)]

# re-order columns
ud_pupils <- ud_pupils[, column_lookup_pupils$standard_name]

# replace NA in LAESTAB where possible
ud_pupils$laestab <- ifelse(is.na(ud_pupils$laestab) & !is.na(ud_pupils$old_la_code) & !is.na(ud_pupils$estab),
                      as.numeric(paste0(ud_pupils$old_la_code, ud_pupils$estab)),
                      ud_pupils$laestab)

# check urns and clean up data
ud_pupils <- cleanup_data(data_in = ud_pupils)


# Print summary of combined dataset
cat("\n--- COMBINED PUPILS DATASET SUMMARY ---\n")
cat("Total rows:", nrow(ud_pupils), "\n")
cat("Total columns:", ncol(ud_pupils), "\n")
cat("Academic years included:", length(unique(ud_pupils$time_period)), "\n")
cat("Years:", paste(sort(unique(ud_pupils$time_period)), collapse = ", "), "\n")

# Show year distribution
year_counts <- table(ud_pupils$time_period)
print(year_counts)

# # fill in number of boys/girls for all single sex schools
# ud_pupils$num_pup_boys <- ifelse(is.na(ud_pupils$num_pup_boys) & !is.na(ud_pupils$num_pup_girls), 0, ud_pupils$num_pup_boys)
# ud_pupils$num_pup_girls <- ifelse(is.na(ud_pupils$num_pup_girls) & !is.na(ud_pupils$num_pup_boys), 0, ud_pupils$num_pup_girls)

# sum up pupil ethnicity #

# Black ethnic origin
new_col <- "num_pup_ethn_black" 
ud_pupils[, new_col] <- rowSums(ud_pupils[, grepl("ethn_black_", names(ud_pupils))], na.rm = T)
ud_pupils$na_count <- rowSums(is.na(ud_pupils[, grepl("ethn_black_", names(ud_pupils))]))
ud_pupils[, new_col] <- ifelse(ud_pupils$na_count == max(ud_pupils$na_count), NA, ud_pupils[, new_col])
ud_pupils[, grepl("ethn_black_", names(ud_pupils))] <- NULL

# Asian ethnic origin
new_col <- "num_pup_ethn_asian" 
ud_pupils[, new_col] <- rowSums(ud_pupils[, grepl("ethn_asian_", names(ud_pupils))], na.rm = T)
ud_pupils$na_count <- rowSums(is.na(ud_pupils[, grepl("ethn_asian_", names(ud_pupils))]))
ud_pupils[, new_col] <- ifelse(ud_pupils$na_count == max(ud_pupils$na_count), NA, ud_pupils[, new_col])
ud_pupils[, grepl("ethn_asian_", names(ud_pupils))] <- NULL

# Mixed ethnic origin
new_col <- "num_pup_ethn_mixed" 
ud_pupils[, new_col] <- rowSums(ud_pupils[, grepl("ethn_mixed_", names(ud_pupils))], na.rm = T)
ud_pupils$na_count <- rowSums(is.na(ud_pupils[, grepl("ethn_mixed_", names(ud_pupils))]))
ud_pupils[, new_col] <- ifelse(ud_pupils$na_count == max(ud_pupils$na_count), NA, ud_pupils[, new_col])
ud_pupils[, grepl("ethn_mixed_", names(ud_pupils))] <- NULL

ud_pupils$na_count <- NULL

# check FSM numbers #

# create df with fsm relevant data only
check <- ud_pupils[, grepl("urn|time|fsm|tot", names(ud_pupils))]

# check data availability by year
check %>% group_by(time_period) %>%
  summarise(
    rows = sum(!is.na(urn_ud_pupils)),
    
    num_pup_fsm = sum(is.na(num_pup_fsm)),
    perc_pup_fsm = sum(is.na(perc_pup_fsm)),
    num_pup_tot_fsm_calc = sum(is.na(num_pup_tot_fsm_calc)),
    num_pup_fsm_performance_tables = sum(is.na(num_pup_fsm_performance_tables)),
    perc_pup_fsm_performance_tables = sum(is.na(perc_pup_fsm_performance_tables)),
    num_pup_tot_fsm_calc_performance_tables = sum(is.na(num_pup_tot_fsm_calc_performance_tables))
  )

# num/perc pupils eligible for FSM #
# available for all years
# if num is known, perc is known
# but total number of pupils used for the FSM calc only available between 2010/11 - 2012/13
# because the total number used in the calculations is not known, we cannot recompute the percentages with more precision for all years

# methodology change for reporting the percentage of pupils known to be eligible for FSM in the SPC data release #

# Up to and including the 2012/13 academic year, the DfE included a variable in the underlying school-level data called ‘number of pupils (used for FSM calculation)’.
# From 2013/14 onwards, this variable no longer appears, and the published percentage of pupils eligible for FSM is calculated using the total headcount of pupils.
# NOTE: More temporally consistent measure of perc pup FSM should use total number of pupils as denominator throughout!! 

# Pre 2012/13:
# The FSM percentage was calculated using a specific denominator: the ‘number of pupils (used for FSM calculation)’.
# This denominator excluded some pupils who were not eligible for FSM by definition or for whom FSM status was not collected. e.g., pupils in nursery classes, some post-16 pupils
# FSM percentage reflected the proportion of eligible pupils among only those for whom FSM eligibility was assessed.

# Post 2012/13:
# The DfE switched to using the total headcount of pupils as the denominator for FSM calculations.
# This means the FSM percentage now reflects the proportion of eligible pupils among all pupils on roll, regardless of whether FSM status was collected for every pupil.
# This change aligned the calculation with other headline measures and simplified reporting, but it may have slightly altered the FSM percentage, particularly in schools with large numbers of pupils in nursery or post-16 provision.

# re-calculate perc of pupils eligible for FSM
check$perc_pup_fsm_RECALC <- ifelse(check$time_period %in% c(201011, 201112, 201213), round(check$num_pup_fsm / check$num_pup_tot_fsm_calc * 100, 1),
                                    round(check$num_pup_fsm / check$num_pup_total * 100, 1))
# compute difference
check$diff_perc_pup_fsm <- check$perc_pup_fsm - check$perc_pup_fsm_RECALC

# check descriptives of diff
psych::describe(check$diff_perc_pup_fsm)


# num/perc pupils eligible for FSM (performance tables) #

# re-calculate perc of pupils eligible for FSM (performance tables)
check$perc_pup_fsm_performance_tables_RECALC <- round(check$num_pup_fsm_performance_tables / check$num_pup_tot_fsm_calc_performance_tables * 100, 1)

# compute difference
check$diff_perc_pup_fsm_performance_tables <- check$perc_pup_fsm_performance_tables - check$perc_pup_fsm_performance_tables_RECALC

# check descriptives of diff
psych::describe(check$diff_perc_pup_fsm_performance_tables)

# Compare FSM numbers in SPC vs performance tables #

# FSM (census): Includes all pupils on roll, including nursery and post-16.
# FSM (performance tables): Only includes pupils in ‘eligible year groups’ (reception to year 11, or 1 to 11 in special schools).

# count
check$diff_num_pup_fsm <- check$num_pup_fsm - check$num_pup_fsm_performance_tables
psych::describeBy(check$diff_num_pup_fsm, group = check$time_period)

# percentage
check$diff_perc_pup_fsm <- check$perc_pup_fsm - check$perc_pup_fsm_performance_tables
psych::describeBy(check$diff_perc_pup_fsm, group = check$time_period)



  
# class size #

# list data files
files_class_size <- list.files(path = dir_in,
                            pattern = "class|Class",
                            recursive = T,
                            full.names = T)
files_class_size <- files_class_size[!grepl(".pdf|.xls|/data/class|/data/spc", files_class_size)]
files_class_size

# Class size dataset column lookup table - basic identifiers and overall measures only
column_lookup_class_size <- tibble(
  standard_name = c(
    # Basic identifiers (specified ones only)
    "urn", "old_la_code", "estab", "laestab", "school_name", "time_period",
    
    # Overall class size measures only
    "num_one_teacher_classes_size_1_30",
    "num_one_teacher_classes_size_31_35", 
    "num_one_teacher_classes_size_36_plus",
    "total_num_one_teacher_class_size",
    "num_pup_in_one_teacher_classes_size_1_30",
    "num_pup_in_one_teacher_classes_size_31_35",
    "num_pup_in_one_teacher_classes_size_36_plus", 
    "total_num_pup_in_one_teacher_classes",
    "average_size_one_teacher_classes"
  ),
  
  # All possible variations for each standard name
  variations = list(
    # Basic identifiers - handle variations across years
    c("urn"),
    c("old_la_code"),
    c("estab", "estab_number"),
    c("laestab", "laestab_no_"),
    c("school_name"),
    c("time_period"),
    
    # Overall class size measures - handle size notation variations (1-30, 31-35, 36+)
    c("number_of_classes_of_size_1-30_taught_by_one_teacher"),
    c("number_of_classes_of_size_31-35_taught_by_one_teacher"),
    c("number_of_classes_of_size_36+_taught_by_one_teacher"),
    c("total_number_of_classes_taught_by_one_teacher"),
    c("number_of_pupils_in_classes_of_size_1-30_taught_by_one_teacher"),
    c("number_of_pupils_in_classes_of_size_31-35_taught_by_one_teacher"),
    c("number_of_pupils_in_classes_of_size_36+_taught_by_one_teacher"),
    c("total_number_of_pupils_in_classes_taught_by_one_teacher"),
    c("average_size_of_one_teacher_classes")
  )
)

# Run the review of the column name lookup
review_lookup_mappings(lookup_table = column_lookup_class_size)
write.csv(apply(column_lookup_class_size, 2, as.character), file = file.path(dir_misc, "meta_spc_classes.csv"), row.names = F)

# Create the reverse lookup
reverse_lookup_class_size <- create_reverse_lookup(column_lookup_class_size)

# Initialize empty list to store all processed datasets
ud_all_c <- list()

# LOOP OVER ALL YEARS 
for (i in seq_along(start:finish)) {
  year = c(start:finish)[i]
  
  # determine academic year
  academic_year <- paste0(year,"-", year+1)
  time_period <- as.numeric(gsub("-20", "", academic_year))
  
  # read in data: Use fread with more robust settings
  tmp_c <- fread(files_class_size[i], 
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
  
  # Convert to data.frame if needed for your pipeline
  tmp_c <- as.data.frame(tmp_c)
  
  # change col names
  names(tmp_c) <- tolower(gsub("X...", "", names(tmp_c), fixed = T))
  names(tmp_c) <- gsub("x..", "perc.", names(tmp_c), fixed = T)
  names(tmp_c) <- gsub(".", "_", names(tmp_c), fixed = T)
  names(tmp_c) <- gsub(" ", "_", names(tmp_c), fixed = T)
  
  # **standardise COLUMN NAMES**
  tmp_c <- standardise_column_names(tmp_c, lookup = reverse_lookup_class_size)
  
  # subset to exclude any non-school level data
  tmp_c <- tmp_c %>% filter(urn != "" & urn != 0)
  
  # remove school that was registered twice
  tmp_c <- tmp_c[!(tmp_c$urn == 143840 & tmp_c$estab == 6008), ]
  
  # add year
  tmp_c$time_period <- time_period
  
  # Store processed dataset in list with academic year as name
  ud_all_c[[academic_year]] <- tmp_c
  
  # Print progress
  cat(paste("Processed", academic_year, "- Rows:", nrow(tmp_c), "Columns:", ncol(tmp_c), "\n"))
  
  rm(tmp_c)
}

# **BIND ALL DATASETS TOGETHER**
# Get all unique column names across all datasets
cols_c <- unique(unlist(lapply(ud_all_c, names)))

# Ensure all datasets have the same columns (fill missing with NA)
ud_stan_c <- lapply(ud_all_c, function(df) {
  missing_cols <- setdiff(cols_c, names(df))
  for (col in missing_cols) {
    df[[col]] <- NA
  }
  return(df[cols_c])  # Reorder columns consistently
})

# Combine all datasets using bind_rows
ud_class_size <- bind_rows(ud_stan_c, .id = "academic_year")

# re-order columns
ud_class_size <- ud_class_size[, column_lookup_class_size$standard_name]

# replace NA in LAESTAB where possible
ud_class_size$laestab <- ifelse(is.na(ud_class_size$laestab) & !is.na(ud_class_size$old_la_code) & !is.na(ud_class_size$estab),
                           as.numeric(paste0(ud_class_size$old_la_code, ud_class_size$estab)),
                           ud_class_size$laestab)

# check urns and clean up data
ud_class_size <- cleanup_data(data_in = ud_class_size)


# Print summary of combined dataset
cat("\n--- COMBINED CLASS SIZE DATASET SUMMARY ---\n")
cat("Total rows:", nrow(ud_class_size), "\n")
cat("Total columns:", ncol(ud_class_size), "\n")
cat("Academic years included:", length(unique(ud_class_size$time_period)), "\n")
cat("Years:", paste(sort(unique(ud_class_size$time_period)), collapse = ", "), "\n")

# Show year distribution
year_counts <- table(ud_pupils$time_period)
print(year_counts)


#### save data ####

gc()

# pupils data # 

# re-order columns
ud_pupils <- ud_pupils %>%
  select(-c(old_la_code, estab)) %>%
  relocate(time_period, school, laestab, urn) %>%
  arrange(laestab, time_period)

# save file
data.table::fwrite(ud_pupils, file = file.path(dir_data, "data_spc_pupils.csv"), row.names = F)

# class size data # 

# re-order columns
ud_class_size <- ud_class_size %>%
  select(-c(old_la_code, estab)) %>%
  relocate(time_period, school, laestab, urn) %>%
  arrange(laestab, time_period)

# save file
data.table::fwrite(ud_class_size, file = file.path(dir_data, "data_spc_classes.csv"), row.names = F)
