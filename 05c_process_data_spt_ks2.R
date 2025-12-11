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
# NE - No entries: the school or college did not enter any pupils or students for the qualifications covered by the measure	
# NA - Not applicable: figures are either not available for the year in question, or the data field is not applicable to this school or college	
# SUPP - Suppressed: In certain circumstances we will suppress an establishment's data. This is usually when there are 5 or fewer pupils or students covered by the measure (10 in the case of destination measures)	
# NP - Not published: for example, we do not publish Progress 8 data for independent schools and independent special schools, or breakdowns by disadvantaged and other pupils for independent schools, independent special schools and non-maintained special schools	
# LOWCOV - Low coverage: shown for the ‘value added’ measure and coverage indicator where schools have less than 50% of pupils included in calculation of the measure
# SP - Small percentage: the number is between 0% and 0.5%
# RE - Redacted: of a reliable estimate and therefore don’t provide a fair measure of performance. For transparency, we publish the headline information for these providers separately in the national achievement rates tables
na_values <- c("SUPP", "NP", "", "NE", "NA", "LOWCOV", "SP", "RE", "NEW", "UNAVAIL")

# Get Information about Schools #

# read in establishment data
gias <- as.data.frame(fread(file.path(dir_data, "data_gias_estab.csv"), encoding = "UTF-8"))

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
  if(year == 2019 | year == 2020 | year == 2021) next
  
  # determine academic year
  academic_year <- paste0(year,"-", year+1)
  time_period <- as.numeric(gsub("-20", "", academic_year))
  
  # determine folder for academic year
  dir_year <- file.path(dir_in, academic_year)
  
  # read in meta data
  file_meta <- list.files(path = dir_year, pattern = "ks2_meta", full.names = T, recursive = T)
  # get meta data
  tmp <- read.csv(file = file_meta)
  names(tmp) <- gsub("X...", "", tolower(names(tmp)), fixed = T)
  
  # rename columns
  names(tmp) <- gsub(".", "", tolower(names(tmp)), fixed = T)
  names(tmp)[names(tmp) == "variable"] <- "fieldname"
  names(tmp)[names(tmp) == "label"] <- "labeldescription"
  
  # thin down columns
  tmp <- tmp[, c("fieldname", "labeldescription")]
  
  # add time period
  tmp$time_period <- time_period
  
  tmp$fieldname <- tolower(tmp$fieldname)
  
  # combine across years
  if (year == start) {
    meta <- tmp
  } else {
    meta <- rbind.all.columns(meta, tmp)
  }
    
}

# save meta data to xlsx
xlsx_file <- file.path(dir_misc, "meta_spt.xlsx")
sheet_name <- "ks2"

if (file.exists(xlsx_file)) {
  wb <- openxlsx::loadWorkbook(xlsx_file) # Load the workbook if it exists
  if (sheet_name %in% names(wb)) openxlsx::removeWorksheet(wb, sheet_name) # Remove the sheet if it already exists
} else {
  wb <- openxlsx::createWorkbook() # Create a new workbook if it does not exist
}
openxlsx::addWorksheet(wb, sheet_name) # Add the new sheet with your data
openxlsx::writeData(wb, sheet_name, meta) # Write object to worksheet
openxlsx::saveWorkbook(wb, xlsx_file, overwrite = TRUE) # Save the workbook (overwriting if necessary)


# Key Stage 2 Performance Tables Column Lookup Table
# Organized by measurement type and time period (2010/11 - 2023/24)
# Contains standardized variable names mapped to original column variations
column_lookup_ks2 <- tibble(
  standard_name = c(
    # ===== BASIC IDENTIFIERS =====
    "urn", "la", "estab", "laestab", "time_period",
    
    # ===== PUPIL COUNTS AND DEMOGRAPHICS =====
    "num_ks2",
    "num_ks2_boys", "num_ks2_girls", 
    "perc_ks2_boys", "perc_ks2_girls", 

    # ===== OVERALL PERFORMANCE MEASURES =====
    # 2010/11-2014/15: Average point scores and level measures
    "overall_aps",                                        # Total Average Point Score
    "overall_avg_level",                                  # Average level per pupil (2012/13-2014/15)
    
    # ===== KEY STAGE 1 PRIOR ATTAINMENT =====
    # Pre-2016 levels system (2010/11-2014/15)
    "ks1_aps", 
    "num_ks2_ks1aps_lo", "perc_ks2_ks1aps_lo",
    "num_ks2_ks1aps_mi", "perc_ks2_ks1aps_mi", 
    "num_ks2_ks1aps_hi", "perc_ks2_ks1aps_hi",
    
    # 2015-16+ new system (scaled scores/expected standards)
    "ks1_ass", 
    "num_ks2_ks1ass_lo", "perc_ks2_ks1ass_lo",
    "num_ks2_ks1ass_mi", "perc_ks2_ks1ass_mi",
    "num_ks2_ks1ass_hi", "perc_ks2_ks1ass_hi",
    "num_ks2_ks1ass_na", "perc_ks2_ks1ass_na",
    
    # 2023 cohort was the last to have statutory KS1 assessment data available for progress calculations. 
    # From 2024, the DfE has shifted to using the Reception Baseline Assessment (RBA) 
    # as the starting point for measuring progress through primary school. 
    # RBA data will not be used for published progress measures until the first relevant cohort reaches the end of KS2 in 2028.
    
    # ===== LANGUAGE CHARACTERISTICS =====
    "num_ks2_eal", "perc_ks2_eal",        # English as Additional Language
    "num_ks2_efl", "perc_ks2_efl",        # English as First Language
    "num_ks2_ufl", "perc_ks2_ufl",        # Unclassified First Language
    "num_ks2_nmob", "perc_ks2_nmob",      # Non-mobile pupils, not availabe in 2010/11
    
    # ===== SEN MEASURES =====
    
    
    # 2010/11 – 2013/14: Old SEN system
    #  senels - Pupils with statements or School Action Plus, Combines highest and mid-level support
    #  tsenelst - Pupils with SEN statements, Highest level (statutory) support
    #  tsenelsap - Pupils with School Action Plus, Mid-level support (external input)
    #  seneln - Pupils with School Action, Lowest level (school-based support)
    
    # 2014/15: Transition year (old and new systems overlap)
    #  tsenelst - Pupils with SEN statements, Highest level, being phased out
    #  tsenelsap - Pupils with School Action Plus, Mid-level, being phased out
    #  seneln - Pupils with School Action, Lowest level, being phased out
    #  tsenelk - Pupils with SEN support, New system: replaces Action/Plus
    #  tsenele - Pupils with EHC plan, New statutory plan, replacing statement
    #  tsenelse - Pupils with statement or EHC plan, Both statutory categories
    #  tsenelapk - Pupils with SEN without statement or EHC plan, Lower-level support (Action/Plus/SEN support)
    
    # 2015/16 – 2018/19: New system bedding in
    #  tsenelapk - Pupils with SEN without statement or EHC plan, Lower-level (SEN support)
    #  tsenele - Pupils with EHC plan, Statutory plan
    #  tsenelk - Pupils with SEN support, School-based support
    #  tsenelst - Pupils with SEN statements, Phasing out (may still appear briefly)
    #  tsenelsap - Pupils with School Action Plus, Phasing out (may still appear briefly)
    #  seneln - Pupils with School Action, Phasing out (may still appear briefly)
    #  tsenelse - Pupils with statement or EHC plan, Both statutory categories
    
    # 2018/19 – Present: Fully new system
    #  tsenelk - Pupils with SEN support, School-based support
    #  tsenele - Pupils with EHC plan, Statutory plan
    #  tsenelek - Pupils with EHC plan or SEN support, All pupils with SEN
    
    
    # Old system pre-2014/15
    "num_ks2_sen_a", "perc_ks2_sen_a",              # School Action (2010/11 - 2017/18)
    "num_ks2_sen_ap", "perc_ks2_sen_ap",            # School Action Plus (2010/11 - 2017/18)
    "num_ks2_sen_aps", "perc_ks2_sen_aps",  # School Action Plus or Statement (2010/11 - 2014/15)
    "num_ks2_sen_st", "perc_ks2_sen_st",        # Statement (2010/11 - 2017/18)
    
    # New system 2014-15plus
    "num_ks2_sen_k", "perc_ks2_sen_k",          # SEN Support (2014/15 - 2023/24)
    "num_ks2_sen_e", "perc_ks2_sen_e",            # EHC Plan (2014/15 - 2023/24)
    "num_ks2_sen_ek", "perc_ks2_sen_ek",    # SEN Support or EHC Plan (2018/19 - 2023/24) = num_ks2_sen_supp + num_ks2_sen_ehc
    "num_ks2_sen_se", "perc_ks2_sen_se",  # EHC Plan or Statement (2014/15 - 2017/18) = num_ks2_sen_ehc + num_ks2_sen_state
    "num_ks2_sen_apk", "perc_ks2_sen_apk",  # SEN without EHC or Statement (2014/15 - 2017/18) = num_ks2_sen_sa + num_ks2_sen_aps + num_ks2_sen_supp
    
    # ===== DISADVANTAGE MEASURES =====
    # FSM/CLA definition pre-2014/15
    "num_ks2_fsmcla", "perc_ks2_fsmcla",
    "num_ks2_not_fsmcla", "perc_ks2_not_fsmcla",
    
    # FSM 6-year definition 2014-15plus
    "num_ks2_fsmcla1a", "perc_ks2_fsmcla1a",
    "num_ks2_not_fsmcla1a", "perc_ks2_not_fsmcla1a",
    
    # ===== ATTAINMENT MEASURES =====
    
    ## COMBINED SUBJECT ACHIEVEMENT ##
    # 2010/11-2014/15: Level-based system
    "att_em_l4plus",                         # Level 4plus English & Mathematics
    "att_em_l4plus_boys", "att_em_l4plus_girls",
    "att_em_l4plus_lo", "att_em_l4plus_mi", "att_em_l4plus_hi",
    "att_em_l4plus_eal", 
    "att_em_l4plus_disadv", "att_em_l4plus_nondisadv",
    
    "att_em_l5plus",                         # Level 5plus English & Mathematics
    "att_em_l5plus_boys", "att_em_l5plus_girls",
    "att_em_l5plus_lo", "att_em_l5plus_mi", "att_em_l5plus_hi",
    "att_em_l5plus_eal", 
    "att_em_l5plus_disadv", "att_em_l5plus_notdisadv",
    
    # 2013/14-2014/15: Reading, Writing TA & Mathematics
    "att_rwm_l4plus",                  # Level 4plus Reading, Writing TA & Mathematics
    "att_rwm_l4plus_boys", "att_rwm_l4plus_girls",
    "att_rwm_l4plus_lo", "att_rwm_l4plus_mi", "att_rwm_l4plus_hi",
    "att_rwm_l4plus_eal",
    "att_rwm_l4plus_disadv", "att_rwm_l4plus_notdisadv",
    "att_rwm_l4plus_gap_nat",                      # Gap between school disadvantaged and national others
    
    # 2012/13-2014/15: Level 4B+ Reading, Writing TA & Mathematics (secure Level 4)
    "att_rwm_l4bplus",                 # Level 4B+ Reading, Writing TA & Mathematics
    "att_rwm_l4bplus_boys", "att_rwm_l4bplus_girls",
    "att_rwm_l4bplus_lo", "att_rwm_l4bplus_mi", "att_rwm_l4bplus_hi",
    "att_rwm_l4bplus_eal",
    "att_rwm_l4bplus_disadv", "att_rwm_l4bplus_notdisadv",
    "att_rwm_l4bplus_gap_nat",                     # Gap between school disadvantaged and national others
    
    "att_rwm_l5plus",                  # Level 5plus Reading, Writing TA & Mathematics
    "att_rwm_l5plus_boys", "att_rwm_l5plus_girls",
    "att_rwm_l5plus_lo", "att_rwm_l5plus_mi", "att_rwm_l5plus_hi",
    "att_rwm_l5plus_eal", 
    "att_rwm_l5plus_disadv", "att_rwm_l5plus_notdisadv",
    
    # 2015/16-2023/24: Expected standards system
    "att_rwm_exp",                               # Expected standard Reading, Writing & Mathematics
    "att_rwm_exp_boys", "att_rwm_exp_girls",
    "att_rwm_exp_lo", "att_rwm_exp_mi", "att_rwm_exp_hi",
    "att_rwm_exp_eal", 
    "att_rwm_exp_disadv", "att_rwm_exp_notdisadv",
    "att_rwm_exp_gap_nat",                                # Difference between disadvantaged and national others
    
    "att_rwm_high",                              # High standard Reading, Writing & Mathematics
    "att_rwm_high_boys", "att_rwm_high_girls",
    "att_rwm_high_lo", "att_rwm_high_mi", "att_rwm_high_hi",
    "att_rwm_high_eal", 
    "att_rwm_high_disadv", "att_rwm_high_notdisadv",
    "att_rwm_high_gap_nat",                               # Difference between disadvantaged and national others
    
    ## INDIVIDUAL SUBJECT ACHIEVEMENT ##
    
    # READING
    # 2010/11-2014/15: Level-based
    "att_read_l4plus", 
    "att_read_l5plus", 
    "att_read_l4plus_lo", "att_read_l4plus_mi", "att_read_l4plus_hi",
    "att_read_l4plus_disadv", "att_read_l4plus_notdisadv",
    
    # 2012/13-2014/15: Level 4B+ (secure Level 4)
    "att_read_l4bplus",
    "att_read_l4bplus_lo", "att_read_l4bplus_mi", "att_read_l4bplus_hi",
    "att_read_l4bplus_disadv", "att_read_l4bplus_notdisadv",
    
    # 2015/16-2023/24: Expected standards
    "att_read_avg_score",
    "att_read_avg_score_boys", "att_read_avg_score_girls",
    "att_read_avg_score_lo", "att_read_avg_score_mi", "att_read_avg_score_hi", # not in 2023/24 onwards
    "att_read_avg_score_eal",
    "att_read_avg_score_disadv",
    "att_read_avg_score_notdisadv",
    "att_read_exp", 
    "att_read_exp_lo", "att_read_exp_mi", "att_read_exp_hi",
    "att_read_exp_disadv", "att_read_exp_notdisadv",
    "att_read_high", 
    "att_read_high_lo", "att_read_high_mi", "att_read_high_hi",
    "att_read_high_disadv", "att_read_high_notdisadv",
    
    # MATHEMATICS
    # 2010/11-2014/15: Level-based
    "att_math_l4plus", 
    "att_math_l5plus", 
    "att_math_l4plus_lo", "att_math_l4plus_mi", "att_math_l4plus_hi",
    "att_math_l4plus_disadv", "att_math_l4plus_notdisadv",
    
    # 2012/13-2014/15: Level 4B+ (secure Level 4)
    "att_math_l4bplus",
    "att_math_l4bplus_lo", "att_math_l4bplus_mi", "att_math_l4bplus_hi",
    "att_math_l4bplus_disadv", "att_math_l4bplus_notdisadv",
    
    # 2015/16-2023/24: Expected standards
    "att_math_avg_score",
    "att_math_avg_score_boys", "att_math_avg_score_girls",
    "att_math_avg_score_lo", "att_math_avg_score_mi", "att_math_avg_score_hi", # not in 2023/24 onwards
    "att_math_avg_score_eal",
    "att_math_avg_score_disadv",
    "att_math_avg_score_notdisadv",
    "att_math_exp",
    "att_math_exp_lo", "att_math_exp_mi", "att_math_exp_hi",
    "att_math_exp_disadv", "att_math_exp_notdisadv",
    "att_math_high",
    "att_math_high_lo", "att_math_high_mi", "att_math_high_hi",
    "att_math_high_disadv", "att_math_high_notdisadv",
    
    # WRITING TA = Teacher Assessed
    # 2013/14-2014/15: Level-based TA
    "att_writ_l4plus",
    "att_writ_l5plus",
    "att_writ_l4plus_lo", "att_writ_l4plus_mi", "att_writ_l4plus_hi",
    "att_writ_l4plus_disadv", "att_writ_l4plus_notdisadv",
    
    # 2015/16-2023/24: Expected standards TA
    "att_writ_exp",
    "att_writ_exp_lo", "att_writ_exp_mi", "att_writ_exp_hi",
    "att_writ_exp_disadv", "att_writ_exp_notdisadv",
    "att_writ_high",
    "att_writ_high_lo", "att_writ_high_mi", "att_writ_high_hi",
    "att_writ_high_disadv", "att_writ_high_notdisadv",
    
    # GRAMMAR, PUNCTUATION & SPELLING
    # 2012/13-2014/15: Level 4B+ GPS (secure Level 4)
    "att_gps_l4bplus",
    "att_gps_l5plus",
    "att_gps_l4bplus_lo", "att_gps_l4bplus_mi", "att_gps_l4bplus_hi",
    "att_gps_l4bplus_disadv", "att_gps_l4bplus_notdisadv",
    
    # 2015/16-2023/24: Expected standards
    "att_gps_avg_score",
    "att_gps_avg_score_boys", "att_gps_avg_score_girls",
    "att_gps_avg_score_lo", "att_gps_avg_score_mi", "att_gps_avg_score_hi", # not in 2023/24 onwards
    "att_gps_avg_score_eal",
    "att_gps_avg_score_disadv",
    "att_gps_avg_score_notdisadv",
    "att_gps_exp",
    "att_gps_exp_lo", "att_gps_exp_mi", "att_gps_exp_hi",
    "att_gps_exp_disadv", "att_gps_exp_notdisadv",
    "att_gps_high",
    "att_gps_high_lo", "att_gps_high_mi", "att_gps_high_hi",
    "att_gps_high_disadv", "att_gps_high_notdisadv",
    
    # SCIENCE (Teacher Assessment only)
    # 2010/11-2014/15: Level-based TA
    "att_scita_l4plus",
    
    # 2015/16-2023/24: Expected standards TA
    "att_scita_exp",
    
    # ===== PROGRESS MEASURES =====
    
    ## SUBJECT-SPECIFIC PROGRESS ##
    
    # ENGLISH PROGRESS (2010/11-2014/15: 2+ levels expected)
    "prog_p2l_eng", "prog_p2l_eng_cov",
    "prog_p2l_eng_boys", "prog_p2l_eng_girls",
    "prog_p2l_eng_lo", "prog_p2l_eng_mi", "prog_p2l_eng_hi",
    "prog_p2l_eng_eal", 
    "prog_p2l_eng_disadv", "prog_p2l_eng_notdisadv",
    
    # MATHEMATICS PROGRESS (2010/11-2014/15: 2+ levels expected)
    "prog_p2l_math", "prog_p2l_math_cov",
    "prog_p2l_math_boys", "prog_p2l_math_girls",
    "prog_p2l_math_lo", "prog_p2l_math_mi", "prog_p2l_math_hi",
    "prog_p2l_math_eal",
    "prog_p2l_math_disadv", "prog_p2l_math_notdisadv",
    "prog_p2l_math_gap_nat",                    # Gap between disadvantaged and national others
    
    # READING PROGRESS (2012/13-2014/15: 2+ levels expected)
    "prog_p2l_read", "prog_p2l_read_cov",
    "prog_p2l_read_boys", "prog_p2l_read_girls",
    "prog_p2l_read_lo", "prog_p2l_read_mi", "prog_p2l_read_hi",
    "prog_p2l_read_eal",
    "prog_p2l_read_disadv", "prog_p2l_read_notdisadv",
    "prog_p2l_read_gap_nat",                    # Gap between disadvantaged and national others
    
    # WRITING TA PROGRESS (2012/13-2014/15: 2+ levels expected)
    "prog_p2l_writ", "prog_p2l_writ_cov",
    "prog_p2l_writ_boys", "prog_p2l_writ_girls",
    "prog_p2l_writ_lo", "prog_p2l_writ_mi", "prog_p2l_writ_hi",
    "prog_p2l_writ_eal",
    "prog_p2l_writ_disadv", "prog_p2l_writ_notdisadv",
    "prog_p2l_writ_gap_nat",                    # Gap between disadvantaged and national others
    
    # NEW PROGRESS MEASURES (2015/16-2023/24: Scaled progress scores with confidence intervals)
    
    # READING PROGRESS (new system)
    "prog_sps_read", "prog_sps_read_cov",
    "prog_sps_read_boys", "prog_sps_read_girls",
    "prog_sps_read_lo", "prog_sps_read_mi", "prog_sps_read_hi",
    "prog_sps_read_eal",
    "prog_sps_read_disadv", "prog_sps_read_notdisadv",
    "prog_sps_read_gap_nat",                   # Difference between disadvantaged and national others
    
    # WRITING PROGRESS (new system)
    "prog_sps_writ", "prog_sps_writ_cov",
    "prog_sps_writ_boys", "prog_sps_writ_girls",
    "prog_sps_writ_lo", "prog_sps_writ_mi", "prog_sps_writ_hi",
    "prog_sps_writ_eal",
    "prog_sps_writ_disadv", "prog_sps_writ_notdisadv",
    "prog_sps_writ_gap_nat",                   # Difference between disadvantaged and national others
    
    # MATHEMATICS PROGRESS (new system)
    "prog_sps_math", "prog_sps_math_cov",
    "prog_sps_math_boys", "prog_sps_math_girls",
    "prog_sps_math_lo", "prog_sps_math_mi", "prog_sps_math_hi",
    "prog_sps_math_eal",
    "prog_sps_math_disadv", "prog_sps_math_notdisadv",
    "prog_sps_math_gap_nat"                     # Difference between disadvantaged and national others
    
  ),
  
  # Variations mapped to standard names
  variations = list(
    # Basic identifiers
    c("urn"),
    c("lea"),
    c("estab"),
    c("laestab"),
    c("time_period"),

    # Pupil counts
    c("telig"),
    c("belig"),
    c("gelig"),
    c("pbelig"),
    c("pgelig"),
    
    # Overall performance measures
    c("taps"),                                           # overall_aps
    c("avglevel"),                                       # overall_avg_level
    
    # KS1 prior attainment (pre-2016)
    c("tks1aps", "tkey stage 1aps"),
    c("tks1exp_l", "tkey stage 1exp_l"),
    c("pks1exp_l", "pkey stage 1exp_l"),
    c("tks1exp_m", "tkey stage 1exp_m"),
    c("pks1exp_m", "pkey stage 1exp_m"),
    c("tks1exp_h", "tkey stage 1exp_h"),
    c("pks1exp_h", "pkey stage 1exp_h"),
    
    # KS1 prior attainment (2015-16+)
    c("tks1average"),
    c("tks1group_l"),
    c("ptks1group_l"),
    c("tks1group_m"),
    c("ptks1group_m"),
    c("tks1group_h"),
    c("ptks1group_h"),
    c("tks1group_na"),
    c("ptks1group_na"),
    
    # Language characteristics
    c("tealgrp2"),
    c("ptealgrp2"),
    c("tealgrp1"),
    c("ptealgrp1"),
    c("tealgrp3"),
    c("ptealgrp3"),
    c("tmobn"),
    c("ptmobn"),
    
    # SEN measures (old system)
    c("seneln"), c("pseneln"),
    c("tsenelsap"), c("psenelsap"),
    c("senels"), c("psenels"),
    c("tsenelst"), c("psenelst"),
    
    # SEN measures (new system)
    c("tsenelk"), c("psenelk"),
    c("tsenele"), c("psenele"),
    c("tsenelek"), c("psenelek"),
    c("tsenelse"), c("psenelse"),
    c("tsenelapk"), c("psenelsapk"),
    
    # Disadvantage measures (old definition)
    c("tfsmcla"),
    c("ptfsmcla"),
    c("tnotfsmcla"),
    c("ptnotfsmcla"),
    
    # Disadvantage measures (new definition)
    c("tfsm6cla1a", "tfsmcla1a"),
    c("ptfsm6cla1a"),
    c("tnotfsm6cla1a"),
    c("ptnotfsm6cla1a"),
    
    # ATTAINMENT MEASURES
    
    # Combined English & Mathematics (Level 4plus)
    c("ptengmatx"),
    c("pbengmatx"), # not in 2010/11, only in 2011/12
    c("pgengmatx"), # not in 2010/11, only in 2011/12
    c("ptengmatx_l"),
    c("ptengmatx_m"),
    c("ptengmatx_h"),
    c("ptengmatx_ealy"), # not in 2010/11, only in 2011/12
    c("ptfsmclaengmatx", "ptengmatx_fsmcla"), # not in 2010/11, only in 2011/12
    c("ptnotfsmclaengmatx", "ptengmatx_notfsmcla"),  # not in 2010/11, only in 2011/12
    
    # Combined English & Mathematics (Level 5plus)
    c("ptengmatax"),
    c("pbengmatax"), # not in 2010/11, only in 2011/12
    c("pgengmatax"), # not in 2010/11, only in 2011/12
    c("ptengmatax_l"),
    c("ptengmatax_m"),
    c("ptengmatax_h"),
    c("ptengmatax_ealy"), # not in 2010/11, only in 2011/12
    c("ptfsmclaengmatax", "ptengmatax_fsmcla"), # not in 2010/11, only in 2011/12
    c("ptnotfsmclaengmatax", "ptengmatax_notfsmcla"), # not in 2010/11, only in 2011/12
    
    # Reading, Writing TA & Mathematics (Level 4plus)
    c("ptreadwrittamatx", "ptreadwritmatx"), # ptreadwritmatx (2010/11) Writing assessed by teacher assessment but writing test still existed and may have been reported in some contexts
    # breakdowns not in 2010/11 - 2011/12, only 2012/13 onwards
    c("pbreadwrittamatx"),
    c("pgreadwrittamatx"),
    c("ptreadwrittamatx_l"),
    c("ptreadwrittamatx_m"),
    c("ptreadwrittamatx_h"),
    c("ptreadwrittamatx_ealy"),
    c("ptreadwrittamatx_fsmcla", "ptreadwrittamatx_fsm6cla1a"),
    c("ptreadwrittamatx_notfsmcla", "ptreadwrittamatx_notfsm6cla1a"),
    c("gapn_rwmx_fsmcla", "gapn_rwmx_fsm6cla1a"), # not in 2010/11 - 2011/12, only 2012/13 onwards
    
    # Reading, Writing TA & Mathematics (Level 4B+) - Introduced 2012/13
    c("ptreadwrittamat4b"),
    c("pbreadwrittamat4b"),
    c("pgreadwrittamat4b"),
    c("ptreadwrittamat4b_l"),
    c("ptreadwrittamat4b_m"),
    c("ptreadwrittamat4b_h"),
    c("ptreadwrittamat4b_ealy"),
    c("ptreadwrittamat4b_fsmcla", "ptreadwrittamat4b_fsm6cla1a"),
    c("ptreadwrittamat4b_notfsmcla", "ptreadwrittamat4b_notfsm6cla1a"),
    c("gapn_rwm4b_fsmcla", "gapn_rwm4b_fsm6cla1a"),
    
    # Reading, Writing TA & Mathematics (Level 5plus)
    c("ptreadwrittamatax", "ptreadwritmatax"), # ptreadwritmatax (2010/11) Writing assessed by teacher assessment but writing test still existed and may have been reported in some contexts
    # breakdowns not in 2010/11 - 2011/12, only 2012/13 onwards
    c("pbreadwrittamatax"),
    c("pgreadwrittamatax"),
    c("ptreadwrittamatax_l"),
    c("ptreadwrittamatax_m"),
    c("ptreadwrittamatax_h"),
    c("ptreadwrittamatax_ealy"),
    c("ptreadwrittamatax_fsmcla"),
    c("ptreadwrittamatax_notfsmcla"),
    
    # Reading, Writing & Mathematics (Expected standard)
    c("ptrwm_exp"),
    c("ptrwm_exp_b"),
    c("ptrwm_exp_g"),
    c("ptrwm_exp_l"), # not in 2023/24 onwards
    c("ptrwm_exp_m"), # not in 2023/24 onwards
    c("ptrwm_exp_h"), # not in 2023/24 onwards
    c("ptrwm_exp_eal"),
    c("ptrwm_exp_fsm6cla1a"),
    c("ptrwm_exp_notfsm6cla1a"),
    c("diffn_rwm_exp"),
    
    # Reading, Writing & Mathematics (High standard)
    c("ptrwm_high"),
    c("ptrwm_high_b"),
    c("ptrwm_high_g"),
    c("ptrwm_high_l"), # not in 2023/24 onwards
    c("ptrwm_high_m"), # not in 2023/24 onwards
    c("ptrwm_high_h"), # not in 2023/24 onwards
    c("ptrwm_high_fsm6cla1a"),
    c("ptrwm_high_notfsm6cla1a"),
    c("ptrwm_high_eal"),
    c("diffn_rwm_high"),
    
    # Individual subjects - Reading (Level 4plus/5plus)
    c("ptreadx"),
    c("ptreadax"),
    # breakdowns not in 2010/11 - 2011/12, only 2012/13 onwards
    c("ptreadx_l"), 
    c("ptreadx_m"),
    c("ptreadx_h"),
    c("ptreadx_fsmcla", "ptreadx_fsm6cla1a"),
    c("ptreadx_notfsmcla", "ptreadx_notfsm6cla1a"),
    
    # Reading (Level 4B+) - Introduced 2012/13
    c("ptread4b"),
    c("ptread4b_l"),
    c("ptread4b_m"),
    c("ptread4b_h"),
    c("ptread4b_fsmcla", "ptread4b_fsm6cla1a"),
    c("ptread4b_notfsmcla", "ptread4b_notfsm6cla1a"),
    
    # Reading (Average scaled scores + Expected/High standard)
    c("read_average"),
    c("read_average_b"),
    c("read_average_g"),
    c("read_average_l"), # not in 2023/24 onwards
    c("read_average_m"), # not in 2023/24 onwards
    c("read_average_h"), # not in 2023/24 onwards
    c("read_average_eal"),
    c("read_average_fsm6cla1a"),
    c("read_average_notfsm6cla1a"),
    
    c("ptread_exp"),
    c("ptread_exp_l"), # not in 2023/24 onwards
    c("ptread_exp_m"), # not in 2023/24 onwards
    c("ptread_exp_h"), # not in 2023/24 onwards
    c("ptread_exp_fsm6cla1a"),
    c("ptread_exp_notfsm6cla1a"),
    
    c("ptread_high"),
    c("ptread_high_l"), # not in 2023/24 onwards
    c("ptread_high_m"), # not in 2023/24 onwards
    c("ptread_high_h"), # not in 2023/24 onwards
    c("ptread_high_fsm6cla1a"),
    c("ptread_high_notfsm6cla1a"),
    
    # Mathematics (Level 4plus/5plus)
    c("ptmatx"),
    c("ptmatax"),
    # breakdowns not in 2010/11 - 2011/12, only 2012/13 onwards
    c("ptmatx_l"),
    c("ptmatx_m"),
    c("ptmatx_h"),
    c("ptmatx_fsmcla", "ptmatx_fsm6cla1a"),
    c("ptmatx_notfsmcla", "ptmatx_notfsm6cla1a"),
    
    # Mathematics (Level 4B+) - Introduced 2012/13
    c("ptmat4b"),
    c("ptmat4b_l"),
    c("ptmat4b_m"),
    c("ptmat4b_h"),
    c("ptmat4b_fsmcla", "ptmat4b_fsm6cla1a"),
    c("ptmat4b_notfsmcla", "ptreadx_notfsm6cla1a"),
    
    # Mathematics (Average scaled scores + Expected/High standard)
    c("mat_average"),
    c("mat_average_b"),
    c("mat_average_g"),
    c("mat_average_l"), # not in 2023/24 onwards
    c("mat_average_m"), # not in 2023/24 onwards
    c("mat_average_h"), # not in 2023/24 onwards
    c("mat_average_eal"),
    c("mat_average_fsm6cla1a"),
    c("mat_average_notfsm6cla1a"),
    
    c("ptmat_exp"),
    c("ptmat_exp_l"), # not in 2023/24 onwards
    c("ptmat_exp_m"), # not in 2023/24 onwards
    c("ptmat_exp_h"), # not in 2023/24 onwards
    c("ptmat_exp_fsm6cla1a"),
    c("ptmat_exp_notfsm6cla1a"),
    
    c("ptmat_high"),
    c("ptmat_high_l"), # not in 2023/24 onwards
    c("ptmat_high_m"), # not in 2023/24 onwards
    c("ptmat_high_h"), # not in 2023/24 onwards
    c("ptmat_high_fsm6cla1a"),
    c("ptmat_high_notfsm6cla1a"),
    
    # Writing TA (Level 4plus/5plus)
    c("ptwritxta", "ptwritx"), # ptwritx (2010/11) Writing assessed by teacher assessment but writing test still existed and may have been reported in some contexts
    c("ptwritaxta", "ptwritax"), # ptwritax (2010/11) Writing assessed by teacher assessment but writing test still existed and may have been reported in some contexts
    # breakdowns not in 2010/11 - 2011/12, only 2012/13 onwards
    c("ptwritxta_l"),
    c("ptwritxta_m"),
    c("ptwritxta_h"),
    c("ptwritxta_fsmcla", "ptwritxta_fsm6cla1a"),
    c("ptwritxta_notfsmcla", "ptwritxta_notfsm6cla1a"),
    
    # Writing TA (Expected/Greater depth)
    c("ptwritta_exp"),
    c("ptwritta_exp_l"), # not in 2023/24 onwards
    c("ptwritta_exp_m"), # not in 2023/24 onwards
    c("ptwritta_exp_h"), # not in 2023/24 onwards
    c("ptwritta_exp_fsm6cla1a"),
    c("ptwritta_exp_notfsm6cla1a"),
    
    c("ptwritta_high"),
    c("ptwritta_high_l"), # not in 2023/24 onwards
    c("ptwritta_high_m"), # not in 2023/24 onwards
    c("ptwritta_high_h"), # not in 2023/24 onwards
    c("ptwritta_high_fsm6cla1a"),
    c("ptwritta_high_notfsm6cla1a"),
    
    # GPS (Level 4B+) - Introduced 2012/13
    c("ptgps4b"),
    c("ptgpsax"),
    c("ptgps4b_l"),
    c("ptgps4b_m"),
    c("ptgps4b_h"),
    c("ptgps4b_fsmcla", "ptgps4b_fsm6cla1a"),
    c("ptgps4b_notfsmcla", "ptgps4b_notfsm6cla1a"),
    
    # GPS (Average scaled scores + Expected/High standard)
    c("gps_average"),
    c("gps_average_b"),
    c("gps_average_g"),
    c("gps_average_l"), # not in 2023/24 onwards
    c("gps_average_m"), # not in 2023/24 onwards
    c("gps_average_h"), # not in 2023/24 onwards
    c("gps_average_eal"),
    c("gps_average_fsm6cla1a"),
    c("gps_average_notfsm6cla1a"),
    
    c("ptgps_exp"),
    c("ptgps_exp_l"), # not in 2023/24 onwards
    c("ptgps_exp_m"), # not in 2023/24 onwards
    c("ptgps_exp_h"), # not in 2023/24 onwards
    c("ptgps_exp_fsm6cla1a"),
    c("ptgps_exp_notfsm6cla1a"),
    
    c("ptgps_high"),
    c("ptgps_high_l"), # not in 2023/24 onwards
    c("ptgps_high_m"), # not in 2023/24 onwards
    c("ptgps_high_h"), # not in 2023/24 onwards
    c("ptgps_high_fsm6cla1a"),
    c("ptgps_high_notfsm6cla1a"),
    
    # Science TA (Level 4plus/5plus)
    c("ptscixta"),
    
    # Science TA (Expected standard)
    c("ptscita_exp"),
    
    # PROGRESS MEASURES
    
    # English progress (2010/11-2011/12)
    c("pt2eng12"),
    c("coveng12"),
    c("pt2eng12_b"),
    c("pt2eng12_g"),
    c("pt2eng12_l"),
    c("pt2eng12_m"),
    c("pt2eng12_h"),
    c("pt2eng12_ealy"),
    c("pt2eng12_fsmcla"),
    c("pt2eng12_notfsmcla"),
    
    # Mathematics progress (2010/11-2014/15)
    c("pt2math12", "pt2math"),
    c("covmath12", "covmath"),
    c("pt2math12_b", "pt2math_b"),
    c("pt2math12_g", "pt2math_g"),
    c("pt2math12_l", "pt2math_l"),
    c("pt2math12_m", "pt2math_m"),
    c("pt2math12_h", "pt2math_h"),
    c("pt2math12_ealy", "pt2math_ealy"),
    c("pt2math12_fsmcla", "pt2math_fsmcla", "pt2math_fsm6cla1a"),
    c("pt2math12_notfsmcla", "pt2math_notfsmcla", "pt2math_notfsm6cla1a"),
    c("gapn_prmat_fsmcla", "gapn_prmat_fsm6cla1a"), # not in 2010/11 - 2011/12, only 2012/13 onwards
    
    # Reading progress (2012/13-2014/15)
    c("pt2read12", "pt2read"),
    c("covread12", "covread"),
    c("pt2read12_b", "pt2read_b"),
    c("pt2read12_g", "pt2read_g"),
    c("pt2read12_l", "pt2read_l"),
    c("pt2read12_m", "pt2read_m"),
    c("pt2read12_h", "pt2read_h"),
    c("pt2read12_ealy", "pt2read_ealy"),
    c("pt2read12_fsmcla", "pt2read_fsmcla", "pt2read_fsm6cla1a"),
    c("pt2read12_notfsmcla", "pt2read_notfsmcla", "pt2read_notfsm6cla1a"),
    c("gapn_prread_fsmcla", "gapn_prread_fsm6cla1a"), # not in 2010/11 - 2011/12, only 2012/13 onwards
    
    # Writing TA progress (2012/13-2014/15)
    c("pt2writta12", "pt2writta"),
    c("covwritta12", "covwritta"),
    c("pt2writta12_b", "pt2writta_b"),
    c("pt2writta12_g", "pt2writta_g"),
    c("pt2writta12_l", "pt2writta_l"),
    c("pt2writta12_m", "pt2writta_m"),
    c("pt2writta12_h", "pt2writta_h"),
    c("pt2writta12_ealy", "pt2writta_ealy"),
    c("pt2writta12_fsmcla", "pt2writta_fsmcla"),
    c("pt2writta12_notfsmcla", "pt2writta_notfsmcla"),
    c("gapn_prwrit_fsmcla", "gapn_prwrit_fsm6cla1a"), # not in 2010/11 - 2011/12, only 2012/13 onwards
    
    # NEW PROGRESS MEASURES (2015/16+), discountinued 2023/24+
    
    # Reading progress (new system) # not in 2023/24 onwards
    c("readprog"),
    c("readcov"),
    c("readprog_b"),
    c("readprog_g"),
    c("readprog_l"),
    c("readprog_m"),
    c("readprog_h"),
    c("readprog_eal"),
    c("readprog_fsm6cla1a"),
    c("readprog_notfsm6cla1a"),
    c("diffn_readprog"),
    
    # Writing progress (new system) # not in 2023/24 onwards
    c("writprog"),
    c("writcov"),
    c("writprog_b"),
    c("writprog_g"),
    c("writprog_l"),
    c("writprog_m"),
    c("writprog_h"),
    c("writprog_eal"),
    c("writprog_fsm6cla1a"),
    c("writprog_notfsm6cla1a"),
    c("diffn_writprog"),
    
    # Mathematics progress (new system) # not in 2023/24 onwards
    c("matprog"),
    c("matcov"),
    c("matprog_b"),
    c("matprog_g"),
    c("matprog_l"),
    c("matprog_m"),
    c("matprog_h"),
    c("matprog_eal"),
    c("matprog_fsm6cla1a"),
    c("matprog_notfsm6cla1a"),
    c("diffn_matprog")
    
  )
)

# Run the review of the column name lookup
review_lookup_mappings(lookup_table = column_lookup_ks2)
write.csv(apply(column_lookup_ks2, 2, as.character), file = file.path(dir_misc, "meta_tmp_ks2.csv"), row.names = F)

# Create the reverse lookup for KS2 data
reverse_lookup_ks2 <- create_reverse_lookup(column_lookup_ks2)


sink = F
if (sink) sink("cols_ks2.txt")

# Initialize empty list to store all processed datasets
df_all <- list()

# LOOP OVER ALL YEARS 
for (i in seq_along(start:finish)) {
  
  year = c(start:finish)[i]  
  
  # skip covid years
  if(year == 2019 | year == 2020 | year == 2021) next
  
  # determine academic year
  academic_year <- paste0(year,"-", year+1)
  time_period <- as.numeric(gsub("-20", "", academic_year))
  
  # determine folder for academic year
  dir_year <- file.path(dir_in, academic_year)
  
  # read in performance data
  file_data <- list.files(path = dir_year, pattern = "england_ks2", full.names = T)
  tmp <- fread(file_data, 
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
  
  # change col names
  names(tmp) <- tolower(gsub("X...", "", names(tmp), fixed = T))
  names(tmp) <- gsub("x..", "perc.", names(tmp), fixed = T)
  names(tmp) <- gsub(".", "_", names(tmp), fixed = T)
  names(tmp) <- gsub(" ", "_", names(tmp), fixed = T)
  
  # add year
  tmp$time_period <- time_period
  
  if (sink) {
    print(academic_year)
    print(names(tmp))
  }
  

  # **standardise COLUMN NAMES**
  tmp <- standardise_column_names(tmp, lookup = reverse_lookup_ks2)
  
  # subset to exclude any non-school level data
  tmp <- tmp %>%
    filter(! is.na(urn))

  # # replace spaces and %
  # tmp <- apply(tmp, 2, function(x) {as.numeric(ifelse(grepl(" |%", x), gsub(" |%", "", x), x))}) %>%
  #   as.data.frame()

  # Store processed dataset in list with academic year (encoded as time_period) as name
  df_all[[academic_year]] <- tmp

  # Print progress
  cat(paste("Processed", academic_year, "- Rows:", nrow(tmp), "Columns:", ncol(tmp), "\n"))

  rm(tmp)
  
}
if (sink) sink()

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
ks2 <- bind_rows(df_stan, .id = "academic_year")

# replace NA in LAESTAB where possible
ks2$laestab <- ifelse(!is.na(ks2$la) & !is.na(ks2$estab),
                             as.numeric(paste0(ks2$la, ks2$estab)),
                             NA)

# check that no column is missing
column_lookup_ks2$standard_name[! column_lookup_ks2$standard_name %in% names(ks2)]

# re-order columns
ks2 <- ks2[, column_lookup_ks2$standard_name]

# Exclude rows that fully consist of NA values
ks2 <- ks2[apply(ks2[, -1:-5], 1, function(row) !all(is.na(row))), ]

# # check for any letters
# sink("check.txt")
# print(apply(ks2, 2, function(x) { unique(regmatches(x, gregexpr("[A-Za-z]+", x)))   }))
# sink()

# Make all columns numeric other than column containing the average level
idx <- which(names(ks2) == "overall_avg_level")
ks2 <- apply(ks2[, -idx], 2, as.numeric) %>% as.data.frame()

# check urns and clean up data
ks2 <- cleanup_data(data_in = ks2)


# Print summary of combined dataset
cat("\n--- COMBINED KS2 DATASET SUMMARY ---\n")
cat("Total rows:", nrow(ks2), "\n")
cat("Total columns:", ncol(ks2), "\n")
cat("Academic years included:", length(unique(ks2$time_period)), "\n")
cat("Years:", paste(sort(unique(ks2$time_period)), collapse = ", "), "\n")

# Show year distribution
year_counts <- table(ks2$time_period)
print(year_counts)

#### FURTHER MODIFY DATA ####

# # fill in number of boys/girls for all single sex schools
# ks2$num_ks2_boys <- ifelse(is.na(ks2$num_ks2_boys) & !is.na(ks2$num_ks2_girls), 0, ks2$num_ks2_boys)
# ks2$num_ks2_girls <- ifelse(is.na(ks2$num_ks2_girls) & !is.na(ks2$num_ks2_boys), 0, ks2$num_ks2_girls)

# Percentage of eligible boys/girls missing in 2010/11, recompute
ks2$perc_ks2_boys <- ifelse(is.na(ks2$perc_ks2_boys), 
                                  round(ks2$num_ks2_boys / ks2$num_ks2_tot * 100), ks2$perc_ks2_boys)
ks2$perc_ks2_girls <- ifelse(is.na(ks2$perc_ks2_girls), 
                                  round(ks2$num_ks2_girls / ks2$num_ks2_tot * 100), ks2$perc_ks2_girls)

# SEN measures

# compute totals
ks2$num_ks2_sen_tot <- rowSums(ks2[, c("num_ks2_sen_a", "num_ks2_sen_ap", "num_ks2_sen_st", "num_ks2_sen_k", "num_ks2_sen_e")], na.rm = T)
ks2$num_ks2_sen_high <- rowSums(ks2[, c("num_ks2_sen_st", "num_ks2_sen_e")], na.rm = T)
ks2$num_ks2_sen_lower <- rowSums(ks2[, c("num_ks2_sen_a", "num_ks2_sen_ap", "num_ks2_sen_k")], na.rm = T)

# compute percentages
levels <- c("tot", "high", "lower")
denominator <- "num_ks2"
for (i in 1:length(levels)) {
  numerator <- paste0("num_ks2_sen_", levels[i])
  quotient <- paste0("perc_ks2_sen_", levels[i])
  ks2[, quotient] <- ks2[, numerator] / ks2[, denominator] * 100
}

# re-order and drop columns
ks2 <- ks2 %>% 
  relocate(any_of(c(paste0("num_ks2_sen_", levels), paste0("perc_ks2_sen_", levels))), .after = perc_ks2_girls) %>%
  arrange(laestab, time_period)

# delete more granular SEN data
levels <- c("a", "ap", "st", "k", "e", "aps", "ek", "se", "apk")
ks2[, paste0("num_ks2_sen_", levels)] <- NULL
ks2[, paste0("perc_ks2_sen_", levels)] <- NULL



#### save data ####

gc()

# re-order columns
ks2 <- ks2 %>% 
  select(-c(la, estab)) %>%
  relocate(time_period, school, laestab, urn) %>%
  arrange(laestab, time_period)

# save file
data.table::fwrite(ks2, file = file.path(dir_data, "tmp_data_spt_ks2.csv"), row.names = F)


#### CREATE TIMESERIES DATA ####

# ===== BASIC IDENTIFIERS =====

var <- "measures"

t1 <- c(201011, 201112, 201213, 201314, 201415)
v1 <- "levels"
t2 <- c(201516, 201617, 201718, 201819, 202223, 202324)
v2 <- "scaled_scores"

ks2[, var] <- ifelse(ks2[, "time_period"] %in% t1, v1, 
                     ifelse(ks2[, "time_period"] %in% t2, v2, NA))

# ===== ATTAINMENT MEASURES =====

## COMBINED SUBJECT ACHIEVEMENT ##

# Headline attainment outcomes #

#  2010/11 to 2011/12 - Percentage achieving Level 4 or above in both English and mathematics (att_em_l4plus)
#  2012/13 to 2014/15 - Percentage achieving Level 4 or above in reading and maths test and writing TA (att_rwm_l4plus)
#  2015/16 to 2023/24 - Percentage of pupils reaching the expected standard in reading, writing and maths (att_rwm_exp)

# create variables
var <- c("att_rwm_head",
         "att_rwm_head_disadv", "att_rwm_head_notdisadv")

t1.1 <- c(201011, 201112)
v1.1 <- c("att_em_l4plus", 
        "att_em_l4plus_disadv", "att_em_l4plus_nondisadv")

t1.2 <- c(201213, 201314, 201415)
v1.2 <- c("att_rwm_l4plus",
        "att_rwm_l4plus_disadv", "att_rwm_l4plus_notdisadv")

v2 <- c("att_rwm_exp",
        "att_rwm_exp_disadv", "att_rwm_exp_notdisadv")

for (i in 1:length(var)) {
  # create new variable
  ks2[, paste0(var[i])] <- 
    ifelse(ks2[, "time_period"] %in% t1.1, ks2[, paste0(v1.1[i])], # 2010/11 - 2011/12
           ifelse(ks2[, "time_period"] %in% t1.2, ks2[, paste0(v1.2[i])], # 2012/13 - 2014/15
                  ifelse(ks2[, "time_period"] %in% t2, ks2[, paste0(v2[i])], NA))) # 2015/16 - 2023/24
  
}

# create tag
var <- "tag_att_headline"

v1.1 <- c("att_em_l4plus")
v1.2 <- c("att_rwm_l4plus")
v2 <- c("att_rwm_exp")


ks2[, var] <- ifelse(ks2[, "time_period"] %in% t1.1, v1.1, 
                     ifelse(ks2[, "time_period"] %in% t1.2, v1.2,
                            ifelse(ks2[, "time_period"] %in% t2, v2, NA)))

# Continuous measure of attainment outcomes #

# Note that from 2015/16, KS2 assessment results are no longer reported as levels: 
# each pupil receives their test results as a scaled score
# expected standard = scaled score of 100 or above
# higher standard = scaled score of 110 or above
# The new expected standards were designed to be broadly similar but are not equivalent to an old level 4b.

#  Maximise for similarity across measures (expected standards)
#  2010/11 OR 2012/13 to 2014/15 - Percentage achieving Level 4B or above
#  2015/16 to 2023/24 - Percentage of pupils reaching the expected standard

var <- "tag_att_exp"

v1.2 <- c("att_[rwm/read/math/writ/gps]_l4bplus")
v2 <- c("att_[rwm/read/math/writ/gps]_exp")


ks2[, var] <- ifelse(ks2[, "time_period"] %in% t1.2, v1.2,
                     ifelse(ks2[, "time_period"] %in% t2, v2, NA))

#  Maximise for similarity across measures (high standards)
#  2010/11 to 2014/15 - Percentage achieving Level 5 or above
#  2015/16 to 2023/24 - Percentage of pupils reaching the high standard

var <- "tag_att_high"

v1.2 <- c("att_[rwm/read/math/writ/gps]_l5plus")
v2 <- c("att_[rwm/read/math/writ/gps]_high")


ks2[, var] <- ifelse(ks2[, "time_period"] %in% t1.2, v1.2,
                     ifelse(ks2[, "time_period"] %in% t2, v2, NA))

#  Maximise for number of observations
#  2010/11 to 2014/15 - Percentage achieving Level 4 or above
#  2015/16 to 2023/24 - Percentage of pupils reaching the expected standard

var <- "tag_att_maxobs"

v1 <- c("att_[rwm/read/math/writ/gps]_l4plus")
v2 <- c("att_[rwm/read/math/writ/gps]_exp")


ks2[, var] <- ifelse(ks2[, "time_period"] %in% t1, v1, 
                     ifelse(ks2[, "time_period"] %in% t2, v2, NA))



# ===== PROGRESS MEASURES =====

#  2010/11 OR 2012/13 to 2014/15 - Percentage of pupils making at least 2 levels of progress in [read/math/writ]
#  2015/16 to 2023/24 - [read/math/writ] progress measure

var <- "tag_prog_[read/math/writ]"

v1 <- c("prog_p2l_[read/math/writ]")
v2 <- c("prog_sps_[read/math/writ]")


ks2[, var] <- ifelse(ks2[, "time_period"] %in% t1, v1,
                     ifelse(ks2[, "time_period"] %in% t2, v2, NA))



# Key Stage 2 Performance Tables Column Lookup Table
# Organized by measurement type and time period (2010/11 - 2023/24)
# Contains standardized variable names mapped to original column variations

column_lookup_ks2 <- tibble(
  standard_name = c(
    # ===== BASIC IDENTIFIERS =====
    "time_period", "school", "laestab", "urn", "urn_ks2", "measures",
    
    # ===== PUPIL COUNTS AND DEMOGRAPHICS =====
    "num_ks2",
    "num_ks2_boys", "num_ks2_girls", 
    "perc_ks2_boys", "perc_ks2_girls", 

    # ===== KEY STAGE 1 PRIOR ATTAINMENT =====
    "avg_ks1",
    "num_ks2_lo", "perc_ks2_lo",
    "num_ks2_mi", "perc_ks2_mi", 
    "num_ks2_hi", "perc_ks2_hi",
    
    # 2023 cohort was the last to have statutory KS1 assessment data available for progress calculations. 
    # From 2024, the DfE has shifted to using the Reception Baseline Assessment (RBA) 
    # as the starting point for measuring progress through primary school. 
    # RBA data will not be used for published progress measures until the first relevant cohort reaches the end of KS2 in 2028.
    
    # ===== LANGUAGE CHARACTERISTICS =====
    "num_ks2_eal", "perc_ks2_eal",        # English as Additional Language
    "num_ks2_efl", "perc_ks2_efl",        # English as First Language
    "num_ks2_ufl", "perc_ks2_ufl",        # Unclassified First Language
    "num_ks2_nmob", "perc_ks2_nmob",      # Non-mobile pupils, not availabe in 2010/11
    
    # ===== SEN MEASURES =====
    
    "num_ks2_sen_tot", "perc_ks2_sen_tot",
    "num_ks2_sen_high", "perc_ks2_sen_high",
    "num_ks2_sen_lower", "perc_ks2_sen_lower",
    
    # ===== DISADVANTAGE MEASURES =====
    "num_ks2_disadv", "perc_ks2_disadv",
    "num_ks2_notdisadv", "perc_ks2_notdisadv",
    
    # ===== ATTAINMENT MEASURES =====
    
    ## COMBINED SUBJECT ACHIEVEMENT ##
    
    # Headline attainment outcomes #
    
    ## 2010/11 to 2011/12 - Percentage achieving Level 4 or above in both English and mathematics (att_em_l4plus)
    ## 2012/13 to 2014/15 - Percentage achieving Level 4 or above in reading and maths test and writing TA (att_rwm_l4plus)
    ## 2015/16 to 2023/24 - Percentage of pupils reaching the expected standard in reading, writing and maths (att_rwm_exp)
    
    "att_rwm_head",
    "att_rwm_head_disadv", "att_rwm_head_notdisadv",
    "tag_att_headline",
    
    # Continuous measure of attainment outcomes #
    
    # Note that from 2015/16, KS2 assessment results are no longer reported as levels: 
    # each pupil receives their test results as a scaled score
    # expected standard = scaled score of 100 or above
    # higher standard = scaled score of 110 or above
    # The new expected standards were designed to be broadly similar but are not equivalent to an old level 4b.
    
    #  Maximise for similarity across measures (expected standards)
    #  2010/11 to 2011/12 - NA
    #  2012/13 to 2014/15 - Percentage achieving Level 4B or above in reading and maths test and writing TA (att_rwm_l4bplus)
    #  2015/16 to 2023/24 - Percentage of pupils reaching the expected standard in reading, writing and maths (att_rwm_exp)
    
    "att_rwm_exp",
    "att_rwm_exp_boys", "att_rwm_exp_girls",
    "att_rwm_exp_lo", "att_rwm_exp_mi", "att_rwm_exp_hi",
    "att_rwm_exp_eal", 
    "att_rwm_exp_disadv", "att_rwm_exp_nondisadv",
    "att_rwm_gap_nat", # Disadvantage gaps in expected standards
    "tag_att_exp", 
    
    #  Maximise for similarity across measures (high standards)
    #  2010/11 to 2011/12 - NA
    #  2012/13 to 2014/15 - Percentage achieving Level 5 or above in reading and maths test and writing TA (att_rwm_l5plus)
    #  2015/16 to 2023/24 - Percentage of pupils reaching a high standard in reading, writing and maths (att_rwm_high)
    
    "att_rwm_high",
    "att_rwm_high_boys", "att_rwm_high_girls",
    "att_rwm_high_lo", "att_rwm_high_mi", "att_rwm_high_hi",
    "att_rwm_high_eal", 
    "att_rwm_high_disadv", "att_rwm_high_nondisadv",
    "tag_att_high",
    
    # #  Maximise for number of observations
    # #  2010/11 to 2014/15 - Percentage achieving Level 4 or above in reading and maths test and writing TA (att_rwm_l4plus)
    # #  2015/16 to 2023/24 - Percentage of pupils reaching the expected standard in reading, writing and maths (att_rwm_exp)
    # 
    # "att_rwm_maxobs",
    # "att_rwm_maxobs_boys", "att_rwm_maxobs_girls",
    # "att_rwm_maxobs_lo", "att_rwm_maxobs_mi", "att_rwm_maxobs_hi",
    # "att_rwm_maxobs_eal", 
    # "att_rwm_maxobs_disadv", "att_rwm_maxobs_nondisadv",
    # "tag_att_maxobs", 
    
    ## INDIVIDUAL SUBJECT ACHIEVEMENT ##
    
    # READING
    
    #  Maximise for similarity across measures (expected standards)
    "att_read_avg_score",
    "att_read_avg_score_boys", "att_read_avg_score_girls",
    "att_read_avg_score_lo", "att_read_avg_score_mi", "att_read_avg_score_hi", # not in 2023/24 onwards
    "att_read_avg_score_eal",
    "att_read_avg_score_disadv",
    "att_read_avg_score_notdisadv",
    
    "att_read_exp",
    "att_read_exp_disadv",
    "att_read_exp_notdisadv",
    
    #  Maximise for similarity across measures (high standards)
    "att_read_high",
    "att_read_high_disadv",
    "att_read_high_notdisadv",
    
    # #  Maximise for number of observations (headline figure)
    # "att_read_maxobs",
    # "att_read_maxobs_disadv",
    # "att_read_maxobs_notdisadv",
    
    # MATHS
    
    #  Maximise for similarity across measures (expected standards)
    "att_math_avg_score",
    "att_math_avg_score_boys", "att_math_avg_score_girls",
    "att_math_avg_score_lo", "att_math_avg_score_mi", "att_math_avg_score_hi", # not in 2023/24 onwards
    "att_math_avg_score_eal",
    "att_math_avg_score_disadv",
    "att_math_avg_score_notdisadv",
    
    "att_math_exp",
    "att_math_exp_disadv",
    "att_math_exp_notdisadv",
    
    #  Maximise for similarity across measures (high standards)
    "att_math_high",
    "att_math_high_disadv",
    "att_math_high_notdisadv",
    
    # #  Maximise for number of observations (headline figure)
    # "att_math_maxobs",
    # "att_math_maxobs_disadv",
    # "att_math_maxobs_notdisadv",
    
    # WRITING
    
    #  Maximise for number of observations AND similarity across measures (expected standards)
    "att_writ_exp",
    "att_writ_exp_disadv",
    "att_writ_exp_notdisadv",
    
    #  Maximise for number of observations AND similarity across measures (high standards)
    "att_writ_high",
    "att_writ_high_disadv",
    "att_writ_high_notdisadv",
    
    # GPS
    
    #  Maximise for number of observations AND similarity across measures (expected standards)
    "att_gps_avg_score",
    "att_gps_avg_score_boys", "att_gps_avg_score_girls",
    "att_gps_avg_score_lo", "att_gps_avg_score_mi", "att_gps_avg_score_hi", # not in 2023/24 onwards
    "att_gps_avg_score_eal",
    "att_gps_avg_score_disadv",
    "att_gps_avg_score_notdisadv",
    
    "att_gps_exp",
    "att_gps_exp_disadv",
    "att_gps_exp_notdisadv",
    
    #  Maximise for number of observations AND similarity across measures (high standards)
    "att_gps_high",
    "att_gps_high_disadv",
    "att_gps_high_notdisadv",
    
    # ===== PROGRESS MEASURES =====
    
    # READING
    
    "prog_read", "prog_read_cov",
    "prog_read_disadv", "prog_read_notdisadv",
    "prog_read_disadv_gap_nat",
    
    # MATHS
    
    "prog_math", "prog_math_cov",
    "prog_math_disadv", "prog_math_notdisadv",
    "prog_math_disadv_gap_nat",
    
    # WRITING
    
    "prog_writ", "prog_writ_cov",
    "prog_writ_disadv", "prog_writ_notdisadv",
    "prog_writ_disadv_gap_nat"
    
  ),
  
  # Variations mapped to standard names
  variations = list(
    # Basic identifiers
    c("time_period"),
    c("school"),
    c("laestab"),
    c("urn"),
    c("urn_ks2"),
    c("measures"),
    
    # Pupil counts
    c("num_ks2"),
    c("num_ks2_boys"),
    c("num_ks2_girls"),
    c("perc_ks2_boys"),
    c("perc_ks2_girls"),
    
    # KS1 prior attainment (pre-2016 and 2015-16+)
    c("ks1_aps", "ks1_ass"),
    c("num_ks2_ks1aps_lo", "num_ks2_ks1ass_lo"),
    c("perc_ks2_ks1aps_lo", "perc_ks2_ks1ass_lo"),
    c("num_ks2_ks1aps_mi", "num_ks2_ks1ass_mi"),
    c("perc_ks2_ks1aps_mi", "perc_ks2_ks1ass_mi"),
    c("num_ks2_ks1aps_hi", "num_ks2_ks1ass_hi"),
    c("perc_ks2_ks1aps_hi", "perc_ks2_ks1ass_hi"),
    
    # Language characteristics
    c("num_ks2_eal"),
    c("perc_ks2_eal"),
    c("num_ks2_efl"),
    c("perc_ks2_efl"),
    c("num_ks2_ufl"),
    c("perc_ks2_ufl"),
    c("num_ks2_nmob"),
    c("perc_ks2_nmob"),
    
    # SEN measures
    c("num_ks2_sen_tot"), c("perc_ks2_sen_tot"),
    c("num_ks2_sen_high"), c("perc_ks2_sen_high"),
    c("num_ks2_sen_lower"), c("perc_ks2_sen_lower"),
    
    # Disadvantage measures (pre-2014/15 and 2014-15+)
    c("num_ks2_fsmcla", "num_ks2_fsmcla1a"),
    c("perc_ks2_fsmcla", "perc_ks2_fsmcla1a"),
    c("num_ks2_not_fsmcla", "num_ks2_not_fsmcla1a"),
    c("perc_ks2_not_fsmcla", "perc_ks2_not_fsmcla1a"),
    
    # ===== ATTAINMENT MEASURES =====
    
    ## COMBINED SUBJECT ACHIEVEMENT ##
    
    # Headline attainment outcomes #
    
    c("att_rwm_head"),
    c("att_rwm_head_disadv"), 
    c("att_rwm_head_notdisadv"),
    c("tag_att_headline"),
    
    #  Maximise for similarity across measures (expected standards)
    #  2010/11 to 2011/12 - NA
    #  2012/13 to 2014/15 - Percentage achieving Level 4B or above in reading and maths test and writing TA (att_rwm_l4bplus)
    #  2015/16 to 2023/24 - Percentage of pupils reaching the expected standard in reading, writing and maths (att_rwm_exp)
    
    # Reading, Writing TA & Mathematics (Level 4B+) & expected standard
    c("att_rwm_l4bplus", "att_rwm_exp"),
    c("att_rwm_l4bplus_boys", "att_rwm_exp_boys"), 
    c("att_rwm_l4bplus_girls", "att_rwm_exp_girls"), 
    c("att_rwm_l4bplus_lo", "att_rwm_exp_lo"),
    c("att_rwm_l4bplus_mi", "att_rwm_exp_mi"),
    c("att_rwm_l4bplus_hi", "att_rwm_exp_hi"),
    c("att_rwm_l4bplus_eal", "att_rwm_exp_eal"), 
    c("att_rwm_l4bplus_disadv", "att_rwm_exp_disadv"), 
    c("att_rwm_l4bplus_notdisadv", "att_rwm_exp_notdisadv"),  
    #  2012/13 to 2014/15 - Gap between school and national % achieving level 4B or above in reading, writing TA and maths for disadvantaged pupils (att_rwm_l4bplus_gap_nat)
    #  2015/16 to 2023/24 - Difference between school percentage of disavantaged pupils and national percentage of other pupils reaching the expected standard in reading, writing and maths (att_rwm_exp_gap_nat)
    c("att_rwm_l4bplus_gap_nat", "att_rwm_exp_gap_nat"),  
    c("tag_att_exp"),
    
    #  Maximise for similarity across measures (high standards)
    #  2010/11 to 2011/12 - NA
    #  2012/13 to 2014/15 - Percentage achieving Level 5 or above in reading and maths test and writing TA (att_rwm_l5plus)
    #  2015/16 to 2023/24 - Percentage of pupils reaching a high standard in reading, writing and maths (att_rwm_high)
    
    # Reading, Writing TA & Mathematics (Level 5+) & high standards
    c("att_rwm_l5plus", "att_rwm_high"),
    c("att_rwm_l5plus_boys", "att_rwm_high_boys"), 
    c("att_rwm_l5plus_girls", "att_rwm_high_girls"), 
    c("att_rwm_l5plus_lo", "att_rwm_high_lo"),
    c("att_rwm_l5plus_mi", "att_rwm_high_mi"),
    c("att_rwm_l5plus_hi", "att_rwm_high_hi"),
    c("att_rwm_l5plus_eal", "att_rwm_high_eal"), 
    c("att_rwm_l5plus_disadv", "att_rwm_high_disadv"), 
    c("att_rwm_l5plus_notdisadv", "att_rwm_high_notdisadv"),  
    c("tag_att_high"),
    
    # #  Maximise for number of observations
    # #  2010/11 to 2014/15 - Percentage achieving Level 4 or above in reading and maths test and writing TA (att_rwm_l4plus)
    # #  2015/16 to 2023/24 - Percentage of pupils reaching the expected standard in reading, writing and maths (att_rwm_exp)
    # 
    # # Reading, Writing TA & Mathematics (Level 4+) & expected standard
    # c("att_rwm_l4plus", "att_rwm_exp"),
    # c("att_rwm_l4plus_boys", "att_rwm_exp_boys"), 
    # c("att_rwm_l4plus_girls", "att_rwm_exp_girls"), 
    # c("att_rwm_l4plus_lo", "att_rwm_exp_lo"),
    # c("att_rwm_l4plus_mi", "att_rwm_exp_mi"),
    # c("att_rwm_l4plus_hi", "att_rwm_exp_hi"),
    # c("att_rwm_l4plus_eal", "att_rwm_exp_eal"), 
    # c("att_rwm_l4plus_disadv", "att_rwm_exp_disadv"), 
    # c("att_rwm_l4plus_notdisadv", "att_rwm_exp_notdisadv"),  
    # c("tag_att_maxobs"),
    
    ## INDIVIDUAL SUBJECT ACHIEVEMENT ##
    
    # READING
    
    #  Maximise for similarity across measures (expected standards)
    #  2010/11 to 2011/12 - NA
    #  2012/13 to 2014/15 - Percentage achieving Level 4B or above in reading (att_read_l4bplus)
    #  2015/16 to 2023/24 - Percentage of pupils reaching the expected standard in reading (att_read_exp)
    c("att_read_avg_score"),
    c("att_read_avg_score_boys"), 
    c("att_read_avg_score_girls"),
    c("att_read_avg_score_lo"), 
    c("att_read_avg_score_mi"), 
    c("att_read_avg_score_hi"), # not in 2023/24 onwards
    c("att_read_avg_score_eal"),
    c("att_read_avg_score_disadv"),
    c("att_read_avg_score_notdisadv"),
    
    
    c("att_read_l4bplus", "att_read_exp"),
    c("att_read_l4bplus_disadv", "att_read_exp_disadv"),
    c("att_read_l4bplus_notdisadv", "att_read_exp_notdisadv"),
    
    #  Maximise for similarity across measures (high standards)
    #  2010/11 to 2014/15 - Percentage achieving Level 5 or above in reading (att_read_l5plus)
    #  2015/16 to 2023/24 - Percentage of pupils reaching a high standard in reading (att_read_high)
    c("att_read_l5plus", "att_read_high"),
    c("att_read_l5plus_disadv", "att_read_high_disadv"),
    c("att_read_high_notdisadv"),
    
    # #  Maximise for number of observations (headline figure)
    # #  2010/11 to 2014/15 - Percentage achieving Level 4 or above in reading test (att_read_l4plus)
    # #  2015/16 to 2023/24 - Percentage of pupils reaching the expected standard in reading (att_read_exp)
    # c("att_read_l4plus", "att_read_exp"),
    # c("att_read_l4plus_disadv", "att_read_exp_disadv"),
    # c("att_read_l4plus_notdisadv", "att_read_exp_notdisadv"),
    
    # MATHS
    
    #  Maximise for similarity across measures (expected standards)
    #  2010/11 to 2011/12 - NA
    #  2012/13 to 2014/15 - Percentage achieving Level 4B or above in maths (att_math_l4bplus)
    #  2015/16 to 2023/24 - Percentage of pupils reaching the expected standard in maths (att_math_exp)
    c("att_math_avg_score"),
    c("att_math_avg_score_boys"), 
    c("att_math_avg_score_girls"),
    c("att_math_avg_score_lo"), 
    c("att_math_avg_score_mi"), 
    c("att_math_avg_score_hi"), # not in 2023/24 onwards
    c("att_math_avg_score_eal"),
    c("att_math_avg_score_disadv"),
    c("att_math_avg_score_notdisadv"),
    
    c("att_math_l4bplus", "att_math_exp"),
    c("att_math_l4bplus_disadv", "att_math_exp_disadv"),
    c("att_math_l4bplus_notdisadv", "att_math_exp_notdisadv"),
    
    #  Maximise for similarity across measures (high standards)
    #  2010/11 to 2014/15 - Percentage achieving Level 5 or above in maths (att_math_l5plus)
    #  2015/16 to 2023/24 - Percentage of pupils reaching a high standard in maths (att_math_high)
    c("att_math_l5plus", "att_math_high"),
    c("att_math_l5plus_disadv", "att_math_high_disadv"),
    c("att_math_high_notdisadv"),
    
    # #  Maximise for number of observations (headline figure)
    # #  2010/11 to 2014/15 - Percentage achieving Level 4 or above in maths test (att_math_l4plus)
    # #  2015/16 to 2023/24 - Percentage of pupils reaching the expected standard in maths (att_math_exp)
    # c("att_math_l4plus", "att_math_exp"),
    # c("att_math_l4plus_disadv", "att_math_exp_disadv"),
    # c("att_math_l4plus_notdisadv", "att_math_exp_notdisadv"),
    
    # WRITING
    
    #  Maximise for number of observations AND similarity across measures (expected standards)
    #  2010/11 to 2014/15 - Percentage achieving Level 4 or above in writing test (att_writ_l4plus)
    #  2015/16 to 2023/24 - Percentage of pupils reaching the expected standard in writing (att_writ_exp)
    c("att_writ_l4plus", "att_writ_exp"),
    c("att_writ_l4plus_disadv", "att_writ_exp_disadv"),
    c("att_writ_l4plus_notdisadv", "att_writ_exp_notdisadv"),
    
    #  Maximise for number of observations AND similarity across measures (high standards)
    #  2010/11 to 2014/15 - Percentage achieving Level 5 or above in writing (att_writ_l5plus)
    #  2015/16 to 2023/24 - Percentage of pupils reaching the expected standard in writing (att_writ_high)
    c("att_writ_l5plus", "att_writ_high"),
    c("att_writ_l5plus_disadv", "att_writ_high_disadv"),
    c("att_writ_high_notdisadv"),
    
    
    # GRAMMAR, PUNCTUATION & SPELLING
    
    #  Maximise for number of observations AND similarity across measures (expected standards)
    #  2010/11 to 2011/12 - NA [no gps measure]
    #  2012/13 to 2014/15 - Percentage achieving Level 4B or above in gps test (att_gps_l4bplus)
    #  2015/16 to 2023/24 - Percentage of pupils reaching the expected standard in gps (att_gps_exp)
    c("att_gps_avg_score"),
    c("att_gps_avg_score_boys"), 
    c("att_gps_avg_score_girls"),
    c("att_gps_avg_score_lo"), 
    c("att_gps_avg_score_mi"), 
    c("att_gps_avg_score_hi"), # not in 2023/24 onwards
    c("att_gps_avg_score_eal"),
    c("att_gps_avg_score_disadv"),
    c("att_gps_avg_score_notdisadv"),
    c("att_gps_l4bplus", "att_gps_exp"),
    c("att_gps_l4bplus_disadv", "att_gps_exp_disadv"),
    c("att_gps_l4bplus_notdisadv", "att_gps_exp_notdisadv"),
    
    #  Maximise for number of observations AND similarity across measures (high standards)
    #  2010/11 to 2011/12 - NA [no gps measure]
    #  2012/13 to 2014/15 - Percentage achieving Level 5 or above in gps (att_gps_l5plus)
    #  2015/16 to 2023/24 - Percentage of pupils reaching a high standard in gps (att_gps_high)
    c("att_gps_l5plus", "att_gps_high"),
    c("att_gps_l5plus_disadv", "att_gps_high_disadv"),
    c("att_gps_high_notdisadv"),
    
    
    # ===== PROGRESS MEASURES =====
    
    # READING
    
    
    #  2010/11 to 2011/12 - NA [progress measure for English only]
    #  2012/13 to 2014/15 - Percentage of pupils making at least 2 levels of progress in reading (prog_p2l_read)
    #  2015/16 to 2023/24 - Reading progress measure (prog_sps_read)
    
    #                           Coverage                Gap
    #  2012/13 to 2014/15   |   prog_p2l_read_cov   |   prog_p2l_read_gap_nat
    #  2015/16 to 2023/24   |   prog_sps_read_cov   |   prog_sps_read_gap_nat
    # Coverage: Percentage of pupils included in progress measure
    # Gap: Difference between progress measure for disadvantaged pupils in school and other pupils nationally
    
    c("prog_p2l_read", "prog_sps_read"),
    c("prog_p2l_read_cov", "prog_sps_read_cov"),
    c("prog_p2l_read_disadv", "prog_sps_read_disadv"),
    c("prog_p2l_read_notdisadv", "prog_sps_read_notdisadv"),
    c("prog_p2l_read_gap_nat", "prog_sps_read_gap_nat"),
    
    # MATHS
    
    #  2010/11 to 2014/15 - Percentage of pupils making at least 2 levels of progress in maths (prog_p2l_math)
    #  2015/16 to 2023/24 - Reading progress measure (prog_sps_math)
    
    #                           Coverage                Gap
    #  2010/11 to 2014/15   |   prog_p2l_math_cov   |   prog_p2l_math_gap_nat
    #  2015/16 to 2023/24   |   prog_sps_math_cov   |   prog_sps_math_gap_nat
    # Coverage: Percentage of pupils included in progress measure
    # Gap: Difference between progress measure for disadvantaged pupils in school and other pupils nationally
    
    c("prog_p2l_math", "prog_sps_math"),
    c("prog_p2l_math_cov", "prog_sps_math_cov"),
    c("prog_p2l_math_disadv", "prog_sps_math_disadv"),
    c("prog_p2l_math_notdisadv", "prog_sps_math_notdisadv"),
    c("prog_p2l_math_gap_nat", "prog_sps_math_gap_nat"),
    
    # WRITING
    
    #  2010/11 to 2011/12 - NA [progress measure for English only]
    #  2012/13 to 2014/15 - Percentage of pupils making at least 2 levels of progress in writing (prog_p2l_writ)
    #  2015/16 to 2023/24 - Reading progress measure (prog_sps_writ)
    
    #                           Coverage                Gap
    #  2012/13 to 2014/15   |   prog_p2l_writ_cov   |   prog_p2l_writ_gap_nat
    #  2015/16 to 2023/24   |   prog_sps_writ_cov   |   prog_sps_writ_gap_nat
    # Coverage: Percentage of pupils included in progress measure
    # Gap: Difference between progress measure for disadvantaged pupils in school and other pupils nationally
    
    c("prog_p2l_writ", "prog_sps_writ"),
    c("prog_p2l_writ_cov", "prog_sps_writ_cov"),
    c("prog_p2l_writ_disadv", "prog_sps_writ_disadv"),
    c("prog_p2l_writ_notdisadv", "prog_sps_writ_notdisadv"),
    c("prog_p2l_writ_gap_nat", "prog_sps_writ_gap_nat")
    
  )
)

# Run the review of the column name lookup
review_lookup_mappings(lookup_table = column_lookup_ks2)
write.csv(apply(column_lookup_ks2, 2, as.character), file = file.path(dir_misc, "meta_ks2.csv"), row.names = F)

# Create the reverse lookup for KS2 data
reverse_lookup_ks2 <- create_reverse_lookup(column_lookup_ks2)

# Initialize empty list to store all processed datasets
df_all <- list()

# LOOP OVER ALL YEARS 
for (i in seq_along(start:finish)) {
  
  year = c(start:finish)[i]  
  
  # skip covid years
  if(year == 2019 | year == 2020 | year == 2021) next
  
  # determine academic year
  academic_year <- paste0(year,"-", year+1)
  time_period <- as.numeric(gsub("-20", "", academic_year))
  
  # subset data
  tmp <- ks2[ks2$time_period == time_period, ]
  
  # Drop columns that are all NA
  tmp <- tmp[, !sapply(tmp, function(x) all(is.na(x)))]
  
  # **standardise COLUMN NAMES**
  tmp <- standardise_column_names(tmp, lookup = reverse_lookup_ks2)
  
  # subset to exclude any non-school level data
  tmp <- tmp %>%
    filter(! is.na(urn))
  
  # # replace spaces and %
  # tmp <- apply(tmp, 2, function(x) {as.numeric(ifelse(grepl(" |%", x), gsub(" |%", "", x), x))}) %>%
  #   as.data.frame()
  
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
df <- bind_rows(df_stan, .id = "academic_year")

# check that no column is missing
column_lookup_ks2$standard_name[! column_lookup_ks2$standard_name %in% names(df)]

# re-order columns
df <- df[, column_lookup_ks2$standard_name]

#### save data ####

gc()

# re-order columns
df <- df %>% 
  relocate(time_period, school, laestab, urn) %>%
  arrange(laestab, time_period)

# save file
data.table::fwrite(df, file = file.path(dir_data, "data_spt_ks2.csv"), row.names = F)
