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
  if(year == 2019 | year == 2020) next
  
  # determine academic year
  academic_year <- paste0(year,"-", year+1)
  time_period <- as.numeric(gsub("-20", "", academic_year))
  
  # determine folder for academic year
  dir_year <- file.path(dir_in, academic_year)
  
  if (year %in% c(2021:2023)) pattern <- "ks4_meta.xlsx" else pattern <- "ks4_meta.csv"
  
  # read in meta data
  file_meta <- list.files(path = dir_year, pattern = pattern, full.names = T, recursive = T)
  
  # get meta data
  if (year %in% c(2021:2023)) tmp <- xlsx::read.xlsx(file_meta, sheetIndex = 1) else tmp <- read.csv(file = file_meta)
  
  names(tmp) <- gsub("X...", "", tolower(names(tmp)), fixed = T)
  
  # thin down columns
  tmp <- tmp[, 2:3]

  # add time period
  tmp$time_period <- time_period
  
  tmp$metafile.heading <- tolower(tmp$metafile.heading)
  
  # combine across years
  if (year == start) {
    meta <- tmp
  } else {
    meta <- rbind.all.columns(meta, tmp)
  }
    
}

# save meta data to xlsx
xlsx_file <- file.path(dir_misc, "meta_spt.xlsx")
sheet_name <- "ks4"

if (file.exists(xlsx_file)) {
  wb <- openxlsx::loadWorkbook(xlsx_file) # Load the workbook if it exists
  if (sheet_name %in% names(wb)) openxlsx::removeWorksheet(wb, sheet_name) # Remove the sheet if it already exists
} else {
  wb <- openxlsx::createWorkbook() # Create a new workbook if it does not exist
}
openxlsx::addWorksheet(wb, sheet_name) # Add the new sheet with your data
openxlsx::writeData(wb, sheet_name, meta) # Write object to worksheet
openxlsx::saveWorkbook(wb, xlsx_file, overwrite = TRUE) # Save the workbook (overwriting if necessary)


# Key Stage 4 Performance Tables Column Lookup Table
# Organized by measurement type and time period (2010/11 - 2023/24)
# Contains standardized variable names mapped to original column variations

column_lookup_ks4 <- tibble(
  standard_name = c(
    # ===== BASIC IDENTIFIERS =====
    "urn", "la", "estab", "laestab", "time_period",
    
    # ===== PUPIL COUNTS AND DEMOGRAPHICS =====
    "num_ks4",
    "num_ks4_boys", "num_ks4_girls", 
    "perc_ks4_boys", "perc_ks4_girls", 
    
    # ===== OVERALL PERFORMANCE MEASURES =====
    # 2010/11-2014/15: Average point scores and level measures
    # "overall_aps",                                        # Total Average Point Score
    # "overall_avg_level",                                  # Average level per pupil (2012/13-2014/15)
    
    # ===== KEY STAGE 1 PRIOR ATTAINMENT =====
    
    "ks2_aps", # Pre-2016 levels system (2010/11-2014/15)
    "ks2_ass", # 2015-16+ new system (scaled scores/expected standards)
    "num_ks4_lo", "perc_ks4_lo",
    "num_ks4_mi", "perc_ks4_mi", 
    "num_ks4_hi", "perc_ks4_hi",
    
    # 2023 cohort was the last to have statutory KS1 assessment data available for progress calculations. 
    # From 2024, the DfE has shifted to using the Reception Baseline Assessment (RBA) 
    # as the starting point for measuring progress through primary school. 
    # RBA data will not be used for published progress measures until the first relevant cohort reaches the end of KS2 in 2028.
    
    # ===== LANGUAGE CHARACTERISTICS =====
    "num_ks4_eal", "perc_ks4_eal",        # English as Additional Language
    "num_ks4_efl", "perc_ks4_efl",        # English as First Language
    "num_ks4_ufl", "perc_ks4_ufl",        # Unclassified First Language
    "num_ks4_nmob", "perc_ks4_nmob",      # Non-mobile pupils, not availabe in 2010/11
    
    # ===== SEN MEASURES =====
    
    # pre-2014 SEN system:
    #   School Action (lowest level, not reported in your list)
    #   School Action Plus (additional external support, reported as senap4)
    #   Statement of SEN (highest level, reported as senst4)
    # 2010/11 to 2013/14
    #   senaps4: Statements of SEN or on School Action Plus; correctly groups the two highest levels, as was common in analysis at the time
    #   senst4: SEN statements
    #   senap4: School Action Plus
    
    # transition year
    # Statements were being replaced by EHC plans (from September 2014).
    # School Action and School Action Plus were replaced by SEN Support.
    # 2014/15
    #   sense4: Statement or EHC plan, reflects the introduction of EHC plans
    #   senaps4: Statement or School Action Plus
    #   senst4: Statement
    #   senap4: School Action Plus
    #   senk4: SEN support reflects the new post-2014 support level
    #   sene4: EHC plan, reflects the introduction of EHC plans
    #   senapk4: SEN without statement or EHC plan correctly groups those on School Action/Plus or SEN Support
    
    # bedding-in of the new system:
    #   Statements were being phased out, EHC plans phased in.
    #   SEN Support replaced School Action/Plus.
    # 2015/16 to 2018/19
    #   sense4: Statement or EHC plan (combines both statement and EHC plan as the highest level)
    #  senapk4: SEN without statement or EHC plan (lower levels of support)
    
    # current system
    #   SEN Support (coded as “K”)
    #   EHC plan (coded as “E”), note: statements have been fully replaced by EHC plans
    # 2021/22 onwards
    #   sene4: EHC plan
    #   senk4: SEN support (without an EHC plan)
    
    
    # Years	              Highest level           Middle/Lower level            Grouped variables             Transition variables
    #   2010/11–2013/14   senst4            	    senap4                    	  senaps4 (statement or SAP)
    #   2014/15	          senst4, sene4, sense4   senap4, senk4	                senaps4, sense4               senapk4 
    #   2015/16–2018/19   sense4	                senapk4	
    #   2021/22 onwards 	sene4                   senk4 (SEN support)
    
    
    # School Action Plus or Statement (2010/11 - 2014/15)
    # combines those with a statement and those on School Action Plus, reflecting the two highest pre-2014 support levels
    "num_ks4_sen_aps", "perc_ks4_sen_aps", # Special Educational Needs with a statement or on School Action Plus at Key Stage 4, ap = action plus, s = statement
    
    # Statement (2010/11 - 2014/15)
    # identifies pupils with a statement, the highest level of statutory support under the old system
    "num_ks4_sen_st", "perc_ks4_sen_st", # Special Educational Needs Statement at Key Stage 4, st = statement
    
    # School Action Plus (2010/11 - 2014/15)
    # Identifies pupils on School Action Plus, a level of support for more complex needs than standard School Action but below a statement
    "num_ks4_sen_ap", "perc_ks4_sen_ap", # Special Educational Needs Action Plus at Key Stage 4, ap = action plus
    
    # EHC Plan or Statement (2014/15 - 2018/19)
    # groups together pupils at the highest level of statutory sen_ support, whether under the old (statement) or new (EHC plan) system
    "num_ks4_sen_se", "perc_ks4_sen_se", # Special Educational Needs Statement or EHC plan at Key Stage 4, se = statement or EHC plan, 
    
    # sen_ support or SAP without EHC or Statement (2014/15 - 2018/19)
    # groups together all pupils with sen_ who do not have a statement or EHC plan, including those on School Action Plus (old system) and sen_ Support (new system)
    "num_ks4_sen_apk", "perc_ks4_sen_apk", # Special Educational Needs at School Action Plus or sen_ Support at Key Stage 4, ap = action plus, k = sen_ support (code “K”)
    
    # sen_ Support (2014/15 & 2021/22 - 2023/24)
    # “K” is the code for pupils receiving sen_ Support under the current (post-2014) system
    "num_ks4_sen_k", "perc_ks4_sen_k", # Special Educational Needs Support at Key Stage 4, k = sen_ support (from the term “K” used in data coding for this group)
    
    # EHC Plan (2014/15 & 2021/22 - 2023/24)
    # Identifies pupils with EHC plan
    "num_ks4_sen_e", "perc_ks4_sen_e", # Special Educational Needs EHC plan at Key Stage 4, e = EHC plan
    
    # SEN Support with or without EHC plan (2021/22 - 2023/24)
    # identifies pupils with special educational needs (SEN) including those with or without an EHC plan
    "num_ks4_sen_all", "perc_ks4_sen_all",
    
    # # Old system pre-2014/15
    # "num_ks4_sen_sap", "perc_ks4_sen_sap",            # School Action Plus (2010/11 - 2014/15)
    # "num_ks4_sen_sap_or_state", "perc_ks4_sen_sap_or_state",  # School Action Plus or Statement (2010/11 - 2014/15)
    # "num_ks4_sen_state", "perc_ks4_sen_state",        # Statement (2010/11 - 2014/15)
    # 
    # # New system 2014-15plus
    # "num_ks4_sen_supp", "perc_ks4_sen_supp",          # SEN Support (2014/15 & 2021/22 - 2023/24)
    # "num_ks4_sen_ehc", "perc_ks4_sen_ehc",            # EHC Plan (2014/15 & 2021/22 - 2023/24)
    # "num_ks4_sen_ehc_or_state", "perc_ks4_sen_ehc_or_state",  # EHC Plan or Statement (2014/15 - 2018/19)
    # "num_ks4_sen_no_ehc_or_state", "perc_ks4_sen_no_ehc_or_state",  # SEN without EHC or Statement (2014/15 - 2018/19)
    # "num_ks4_sen_all", "perc_ks4_sen_all", # SEN with AND without EHC or Statement (2018/19 - 2023/24)
    
    
    # # SEN measures (old system)
    # c("senap4"), c("psenap4"),
    # c("senaps4"), c("psenaps4"),
    # c("senst4"), c("psenst4"),
    # 
    # # SEN measures (new system)
    # c("senk4"), c("psenk4"),
    # c("sene4"), c("psene4"),
    # c("sense4"), c("psense4"), #
    # c("senapk4"), c("psenapk4"),
    # c("sen_all", "sen_all4"), c("psen_all", "psen_all4"),
    
    
    # ===== DISADVANTAGE MEASURES =====
    # FSM/CLA definition pre-2014/15
    "num_ks4_fsmcla", "perc_ks4_fsmcla",
    "num_ks4_not_fsmcla", "perc_ks4_not_fsmcla",
    
    # FSM 6-year definition 2014-15plus
    "num_ks4_fsmcla1a", "perc_ks4_fsmcla1a",
    "num_ks4_not_fsmcla1a", "perc_ks4_not_fsmcla1a",
    
    # ===== ATTAINMENT MEASURES =====
    
    ## GCSE - General Certificate of Secondary Education ##
    
    # Overall good set of GCSE results
    
    # 2010/11-2015/16
    "gcse5em_ac", # Percentage of pupils achieving 5+ A*-C or equivalents including A*-C in both English and mathematics GCSEs
    # after 2016/17
    "gcse5em_94", # Percentage of pupils achieving 5+ A*-C/9-4 or equivalents including 9-4 in both English and mathematics GCSEs
    
    # GCSE results for English AND maths
    
    "em_ac", # Percentage of Key Stage 4 pupils achieving grades A*-C in both English and mathematics GCSEs
    "em_94", # Percentage of pupils achieving standard passes (grades 9-4) in both English and mathematics GCSEs
    "em_95", # Percentage of pupils achieving strong passes (grades 9-5) in both English and mathematics GCSEs
    
    ## English Baccalaureate (EBacc) ##
    
    # measure how many pupils take and do well in a specific group of academic GCSE subjects, not a qualification or certificate per se
    # EBacc subjects: 
    #   English
    #   Mathematics
    #   Sciences (combined science counts as two, or three single sciences: biology, chemistry, physics, or computer science)
    #   A language (modern or ancient)
    #   History or geography
    # EBacc requirements:
    #   GCSE in English
    #   GCSE in Mathematics
    #   1 GCSE in combined science OR 2 GCSEs in seperate sciences
    #   GCSE in a language
    #   GCSE in a humanity
    
    # Entry
    
    # = percentage of pupils who take GCSE exams in all the EBacc subjects
    # shows how many pupils are studying a broad, academic curriculum up to age 16
    
    "ebacc_e", # Percentage of Key Stage 4 pupils entering all English Baccalaureate
    "ebacc_e_lo", "ebacc_e_mi", "ebacc_e_hi",
    "ebacc_e_disadv", "ebacc_e_notdisadv",
    
    # Achievement
    
    # = percentage of pupils who achieve grade 4 (or C) or above in all the EBacc subjects
    # shows how many pupils not only took but also achieved a ‘standard pass’ in all the EBacc subjects. It is a higher bar than entry alone.
    
    # 2010/11-2015/16
    "ebacc", # Percentage of Key Stage 4 pupils achieving the English Baccalaureate
    "ebacc_lo", "ebacc_mi", "ebacc_hi",
    "ebacc_disadv", "ebacc_notdisadv",
    
    # after 2016/17
    "ebacc_94", # Percentage of pupils achieving the English Baccalaureate with standard passes (grades 9-4) in both English and maths and A*-C grades in the remaining elements
    "ebacc_94_disadv", "ebacc_94_notdisadv",
    "ebacc_95", # Percentage of pupils achieving the English Baccalaureate with strong passes (grades 9-5) in both English and maths and A*-C grades in the remaining elements
    "ebacc_95_disadv", "ebacc_95_notdisadv",
    
    # after 2017/18
    "aps_ebacc", # Average EBacc APS score per pupil
    "aps_ebacc_boys", "aps_ebacc_girls",
    "aps_ebacc_lo", "aps_ebacc_mi", "aps_ebacc_hi",
    "aps_ebacc_eal",
    "aps_ebacc_disadv", "aps_ebacc_notdisadv",
    
    
    ## Total average (capped) point score per pupil ##
    
    # = measures how well pupils performed in their best eight qualifications, regardless of subjects
    # Capped: Only the best eight qualifications for each pupil are counted. 
    # Any additional GCSEs or qualifications beyond the best eight do not increase the score.
    # GCSEs only: other types of qualifications (such as BTECs, vocational awards, or equivalents) included from this calculation
    
    # Convert each grade to points (e.g. A* = 58, A = 52, B = 46, C = 40, etc.).
    # Pupil score = add up the points from their eight highest-scoring qualification
    # School score = average of all pupil scores 
    
    # "ttapscp", 
    # "ttapscp_lo", "ttapscp_mi", "ttapscp_hi",
    # "ttapscp_eal",
    # "ttapscp_disadv", "ttapscp_notdisadv",
    
    "caps8", 
    "caps8_lo", "caps8_mi", "caps8_hi",
    "caps8_eal",
    "caps8_disadv", "caps8_notdisadv",
    
    ## Total average (capped) point score per pupil (GCSEs only) ## 
    
    # = measures how well pupils performed in their best eight GCSE subjects
    # GCSEs only: other types of qualifications (such as BTECs, vocational awards, or equivalents) excluded from this calculation
    
    # "ttapsgcp", 
    # "ttapsgcp_lo", "ttapsgcp_mi", "ttapsgcp_hi",
    # "ttapsgcp_eal",
    # "ttapsgcp_disadv", "ttapsgcp_notdisadv",
    
    "caps8gcse", 
    "caps8gcse_lo", "caps8gcse_mi", "caps8gcse_hi",
    "caps8gcse_eal",
    "caps8gcse_disadv", "caps8gcse_notdisadv",
    
    
    ## Attainment 8 - from 2014/15 onwards ## 
    
    # average achievement of pupils across eight key GCSE subjects, using a points system based on the grades they achieve
    # reflects both the range of subjects studied and the grades achieved
    
    # Each GCSE grade is converted into points (e.g. grade 9 = 9 points, grade 8 = 8 points, down to grade 1 = 1 point).
    # Pupil Att8 score = Add up the points for the eight slots. For English and maths, the points are double-weighted (counted twice).
    # school’s Att8 score = average of all pupils’ individual Attainment 8 scores
    
    # subjects included in Att8
    #   English (double-weighted): English language or English literature (the higher grade is double-counted, but both must be taken)
    #   Mathematics (double-weighted)
    #   Three EBacc subjects: Sciences (including computer science), history, geography, or languages
    #   Three other approved subjects: Any remaining GCSEs or approved vocational qualifications (these can include further EBacc subjects or other GCSEs)
    
    # Average Attainment 8 score per pupil
    "att8", 
    "att8_boys", "att8_girls",
    "att8_lo", "att8_mi", "att8_hi",
    "att8_eal",
    "att8_disadv", "att8_notdisadv",
    "att8_gap_nat",
    
    # Average score for different subjects
    
    "att8eng",	# Average Attainment 8 score per pupil for English element
    "att8eng_disadv", "att8eng_notdisadv",
    
    "att8mat",	# Average Attainment 8 score per pupil for mathematics element
    "att8mat_disadv", "att8mat_notdisadv",
    
    # Total sum of Attainment 8 scores 
    
    # extracted to approximate the old capped average point score per pupil, using the Attainment 8 framework
    # (totatt8eng + totatt8mat + totatt8ebac + totatt8openg) / number of att8 pupils
    # number of att8 pupils = round(totatt8 / att8scr)
    
    "totatt8", # sum of attainment 8 scores
    "totatt8eng", # sum of Attainment 8 scores for English element
    "totatt8mat", # sum of Attainment 8 scores for mathematics element
    "totatt8ebac", # sum of Attainment 8 scores for EBacc element
    "totatt8open", # sum of Attainment 8 scores for open element
    "totatt8openg", # sum of Attainment 8 scores for open element - GCSE only
    
    ## Progress 8 - from 2014/15 onwards ## 
    
    # = measure of progress pupils make between the end of KS2 and the end of KS4, across a set of eight key GCSE subjects
    # measures the value a school adds to pupils’ progress, not just their final grades
    
    # Progress 8 compares each pupil’s att8 score to the average att8 score of all pupils nationally who had a similar starting point (based on ks2 results).
    # pupil progress 8 score = difference between their att8 score and the national average for pupils with similar prior attainment.
    # school’s Progress 8 score = average of all its pupils’ progress scores
    #    0 = Pupils, on average, made the same progress as pupils nationally with similar prior attainment
    #   >0 = Pupils, on average, made more progress than similar pupils nationally
    #   <0 = Pupils, on average, made less progress than similar pupils nationally
    
    # Average Progress 8 score per pupil
    "prog8",
    "prog8_cov",
    "prog8_boys", "prog8_girls",
    "prog8_lo", "prog8_mi", "prog8_hi",
    "prog8_eal",
    "prog8_disadv", "prog8_notdisadv",
    "prog8_gap_nat",
    
    # Average score for different subjects
    
    "prog8eng",	# Progress 8 measure for English element
    "prog8eng_disadv", "prog8eng_notdisadv",
    
    "prog8mat",	# Progress 8 measure for mathematics element
    "prog8mat_disadv", "prog8mat_notdisadv"
    
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
    c("tpup"), # Number of pupils at the end of Key Stage 4
    c("bpup"), # Number of boys at the end of key stage 4
    c("gpup"), # Number of girls at the end of key stage 4
    c("pbpup"),
    c("pgpup"),
    
    # KS1 prior attainment
    c("ks2aps"), # (pre-2016)
    c("ks2ass"), # (2015-16+)
    c("tpriorlo"),
    c("ptpriorlo"),
    c("tpriorav"),
    c("ptpriorav"),
    c("tpriorhi"),
    c("ptpriorhi"),
    
    # Language characteristics
    c("tealgrp2"),
    c("ptealgrp2"),
    c("tealgrp1"),
    c("ptealgrp1"),
    c("tealgrp3"),
    c("ptealgrp3"),
    c("tnmob"),
    c("ptnmob"),
    
    # Special educational needs
    
    # School Action Plus or Statement (2010/11 - 2014/15)
    # combines those with a statement and those on School Action Plus, reflecting the two highest pre-2014 support levels
    c("senaps4"), c("psenaps4"), # Special Educational Needs with a statement or on School Action Plus at Key Stage 4, ap = action plus, s = statement
    
    # Statement (2010/11 - 2014/15)
    # identifies pupils with a statement, the highest level of statutory support under the old system
    c("senst4"), c("psenst4"), # Special Educational Needs Statement at Key Stage 4, st = statement
    
    # School Action Plus (2010/11 - 2014/15)
    # Identifies pupils on School Action Plus, a level of support for more complex needs than standard School Action but below a statement
    c("senap4"), c("psenap4"), # Special Educational Needs Action Plus at Key Stage 4, ap = action plus
    
    # EHC Plan or Statement (2014/15 - 2018/19)
    # groups together pupils at the highest level of statutory SEN support, whether under the old (statement) or new (EHC plan) system
    c("sense4"), c("psense4"), # Special Educational Needs Statement or EHC plan at Key Stage 4, se = statement or EHC plan, 
    
    # SEN support or SAP without EHC or Statement (2014/15 - 2018/19)
    # groups together all pupils with SEN who do not have a statement or EHC plan, including those on School Action Plus (old system) and SEN Support (new system)
    c("senapk4"), c("psenapk4"), # Special Educational Needs at School Action Plus or SEN Support at Key Stage 4, ap = action plus, k = SEN support (code “K”)
    
    # SEN Support (2014/15 & 2021/22 - 2023/24)
    # “K” is the code for pupils receiving SEN Support under the current (post-2014) system
    c("senk4"), c("psenk4"), # Special Educational Needs Support at Key Stage 4, k = SEN support (from the term “K” used in data coding for this group)
    
    # EHC Plan (2014/15 & 2021/22 - 2023/24)
    # Identifies pupils with EHC plan
    c("sene4"), c("psene4"), # Special Educational Needs EHC plan at Key Stage 4, e = EHC plan
    
    # SEN Support with or without EHC plan (2021/22 - 2023/24)
    # identifies pupils with special educational needs (SEN) including those with or without an EHC plan
    c("sen_all", "sen_all4"), c("psen_all", "psen_all4"),
    
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
    
    # ===== ATTAINMENT MEASURES =====
    
    ## GCSE ##
    
    # 2010/11-2015/16
    c("ptac5em", "ptac5em_ptq", "ptac5em_ptq_ee"), # Percentage of pupils achieving 5+ A*-C or equivalents including A*-C in both English and mathematics GCSEs
    
    # after 2016/17
    c("pt5em_94"), # Percentage of pupils achieving 5+ A*-C/9-4 or equivalents including 9-4 in both English and mathematics GCSEs
    
    c("ptl2basics", "ptl2basics_ptq", "ptl2basics_ptq_ee", "ptl2basics_ll_ptq_ee"), # Percentage of Key Stage 4 pupils achieving grades A*-C in both English and mathematics GCSEs
    c("ptl2basics_94"), # Percentage of pupils achieving standard passes (grades 9-4) in both English and mathematics GCSEs
    c("ptl2basics_95"), # Percentage of pupils achieving strong passes (grades 9-5) in both English and mathematics GCSEs
    
    ## Entering All English Baccalaureate Subjects ##
    
    c("ptebacc_e", "ptebacc_e_ptq", "ptebacc_e_ptq_ee"), 
    c("ptebacc_elo", "ptebacc_elo_ptq", "ptebacc_elo_ptq_ee"),
    c("ptebacc_eav", "ptebacc_eav_ptq", "ptebacc_eav_ptq_ee"),
    c("ptebacc_ehi", "ptebacc_ehi_ptq", "ptebacc_ehi_ptq_ee"),
    c("ptebacc_efsm", "ptebacc_efsm_ptq", "ptebacc_efsm6cla1a_ptq_ee"),
    c("ptebacc_enfsm", "ptebacc_enfsm_ptq", "ptebacc_enfsm6cla1a_ptq_ee"),
    
    
    ## Achieving English Baccalaureate ##
    
    # 2010/11-2015/16
    c("ptebacc", "ptebacc_ptq", "ptebacc_ptq_ee"), # Percentage of Key Stage 4 pupils achieving the English Baccalaureate
    c("ptebacclo", "ptebacclo_ptq", "ptebacclo_ptq_ee"),
    c("ptebaccav", "ptebaccav_ptq", "ptebaccav_ptq_ee"),
    c("ptebacchi", "ptebacchi_ptq", "ptebacchi_ptq_ee"),
    c("ptebacc_fsm", "ptebacc_fsm_ptq", "ptebacc_fsm6cla1a_ptq_ee"),
    c("ptebacc_nfsm", "ptebacc_nfsm_ptq", "ptebacc_nfsm6cla1a_ptq_ee"),
    
    # after 2016/17
    c("ptebacc_94"), # Percentage of pupils achieving the English Baccalaureate with standard passes (grades 9-4) in both English and maths and A*-C grades in the remaining elements
    c("ptebacc_94_fsm6cla1a"),
    c("ptebacc_94_nfsm6cla1a"),
    c("ptebacc_95"), # Percentage of pupils achieving the English Baccalaureate with strong passes (grades 9-5) in both English and maths and A*-C grades in the remaining elements
    c("ptebacc_95_fsm6cla1a"),
    c("ptebacc_95_nfsm6cla1a"),
    
    c("ebaccaps"), # Average EBacc APS score per pupil
    c("ebaccaps_boys"),
    c("ebaccaps_girls"),
    c("ebaccaps_lo"),
    c("ebaccaps_av", "ebaccaps_mid"),
    c("ebaccaps_hi"),
    c("ebaccaps_eal"),
    c("ebaccaps_fsm6cla1a"),
    c("ebaccaps_nfsm6cla1a"),
    
    ## Capped average point score ##
    
    c("ttapscp", "ttapscp_ptq", "ttapscp_ptq_ee"), # Total average (capped) point score per pupil (all subjects)
    c("ttapscplo", "ttapscplo_ptq", "ttapscplo_ptq_ee"),
    c("ttapscpav", "ttapscpav_ptq", "ttapscpav_ptq_ee"),
    c("ttapscphi", "ttapscphi_ptq", "ttapscphi_ptq_ee"),
    c("ttapscpeal", "ttapscpeal_ptq", "ttapscpeal_ptq_ee"),
    c("ttapscpfsm", "ttapscpfsm_ptq", "ttapscpfsm6cla1a_ptq_ee"),
    c("ttapscpnfsm", "ttapscpnfsm_ptq", "ttapscpnfsm6cla1a_ptq_ee"),
    
    c("ttapsgcp", "ttapsgcp_ptq", "ttapsgcp_ptq_ee"), # Total average (capped) point score per pupil (GCSEs only)
    c("ttapsgcplo", "ttapsgcplo_ptq", "ttapsgcplo_ptq_ee"),
    c("ttapsgcpav", "ttapsgcpav_ptq", "ttapsgcpav_ptq_ee"),
    c("ttapsgcphi", "ttapsgcphi_ptq", "ttapsgcphi_ptq_ee"),
    c("ttapsgcpeal", "ttapsgcpeal_ptq", "ttapsgcpeal_ptq_ee"),
    c("ttapsgcpfsm", "ttapsgcpfsm_ptq", "ttapsgcpfsm6cla1a_ptq_ee"),
    c("ttapsgcpnfsm", "ttapsgcpnfsm_ptq", "ttapsgcpnfsm6cla1a_ptq_ee"),
    
    ## Attainment 8 ##
    
    c("att8scr"), # Average Attainment 8 score per pupil
    c("att8scr_boys"),
    c("att8scr_girls"),
    c("att8scr_lo"),
    c("att8scr_av", "att8scr_mid"),
    c("att8scr_hi"),
    c("att8scr_eal"),
    c("att8scr_fsm6cla1a"),
    c("att8scr_nfsm6cla1a"),
    c("diffn_att8"),
    
    c("att8screng"), # Average Attainment 8 score per pupil for English element
    c("att8screng_fsm6cla1a"),
    c("att8screng_nfsm6cla1a"),
    
    c("att8scrmat"), # Average Attainment 8 score per pupil for mathematics element
    c("att8scrmat_fsm6cla1a"),
    c("att8scrmat_nfsm6cla1a"),
    
    c("totatt8"),  # sum of attainment 8 scores
    c("totatt8eng"),  # sum of Attainment 8 scores for English element
    c("totatt8mat"),  # sum of Attainment 8 scores for mathematics element
    c("totatt8ebac"),  # sum of Attainment 8 scores for EBacc element
    c("totatt8open"),  # sum of Attainment 8 scores for open element
    c("totatt8openg"),  # sum of Attainment 8 scores for open element - GCSE only
    
    ## Progress 8 ##
    
    c("p8mea"), # Progress 8 measure (after adjustment for extreme scores from 2017/18 onwards)
    c("p8meacov"),
    c("p8mea_boys"),
    c("p8mea_girls"),
    c("p8mea_lo"),
    c("p8mea_av", "p8mea_mid"),
    c("p8mea_hi"),
    c("p8mea_eal"),
    c("p8mea_fsm6cla1a"),
    c("p8mea_nfsm6cla1a"),
    c("diffn_p8mea"),
    
    c("p8meaeng"), # Progress 8 measure for English element
    c("p8meaeng_fsm6cla1a"),
    c("p8meaeng_nfsm6cla1a"),
    
    c("p8meamat"), # Progress 8 measure for mathematics element
    c("p8meamat_fsm6cla1a"),
    c("p8meamat_nfsm6cla1a")
    
  )
)

# Run the review of the column name lookup
review_lookup_mappings(lookup_table = column_lookup_ks4)

# Create the reverse lookup for ks4 data
reverse_lookup_ks4 <- create_reverse_lookup(column_lookup_ks4)


sink = F
if (sink) sink("cols_ks4.txt")

# Initialize empty list to store all processed datasets
df_all <- list()

# LOOP OVER ALL YEARS 
for (i in seq_along(start:finish)) {
  
  year = c(start:finish)[i]  
  
  # skip covid years
  if(year == 2019 | year == 2020) next
  
  # determine academic year
  academic_year <- paste0(year,"-", year+1)
  time_period <- as.numeric(gsub("-20", "", academic_year))
  
  # determine folder for academic year
  dir_year <- file.path(dir_in, academic_year)
  
  # read in performance data
  file_data <- list.files(path = dir_year, pattern = "england_ks4", full.names = T)
  tmp <- fread(file_data, 
               fill = Inf,                    # whole file is read for detecting the number of columns.
               sep = ",",                     # Explicitly set separator
               quote = "\"",                  # Handle quoted fields
               strip.white = TRUE,            # Remove whitespace
               blank.lines.skip = TRUE,       # Skip empty lines
               encoding = "Latin-1",          # Handle encoding issues
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
  } else {
  

  # **standardise COLUMN NAMES**
  tmp <- standardise_column_names(tmp, lookup = reverse_lookup_ks4)
  
  # subset to exclude any non-school level data
  tmp <- tmp %>%
    filter(! is.na(urn))

  # replace spaces and %
  tmp <- apply(tmp, 2, function(x) {as.numeric(ifelse(grepl(" |%", x), gsub(" |%", "", x), x))}) %>%
    as.data.frame()

  # Store processed dataset in list with academic year (encoded as time_period) as name
  df_all[[academic_year]] <- tmp

  # Print progress
  cat(paste("Processed", academic_year, "- Rows:", nrow(tmp), "Columns:", ncol(tmp), "\n"))
  
  }

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
ks4 <- bind_rows(df_stan, .id = "academic_year")

# replace NA in LAESTAB where possible
ks4$laestab <- ifelse(!is.na(ks4$la) & !is.na(ks4$estab),
                             as.numeric(paste0(ks4$la, ks4$estab)),
                             NA)

# check that no column is missing
column_lookup_ks4$standard_name[! column_lookup_ks4$standard_name %in% names(ks4)]

# re-order columns
ks4 <- ks4[, column_lookup_ks4$standard_name]

# Exclude rows that fully consist of NA values
# cols 1 - 5: urn, la, estab, laestab, time_period
ks4 <- ks4[apply(ks4[, -1:-5], 1, function(row) !all(is.na(row))), ]

# # check for any letters
# sink("check.txt")
# print(apply(ks4, 2, function(x) { unique(regmatches(x, gregexpr("[A-Za-z]+", x)))   }))
# sink()

# Make all columns numeric other than column containing the average level
ks4 <- apply(ks4, 2, as.numeric) %>% as.data.frame()

# check urns and clean up data
ks4 <- cleanup_data(data_in = ks4)


# Print summary of combined dataset
cat("\n--- COMBINED KS4 DATASET SUMMARY ---\n")
cat("Total rows:", nrow(ks4), "\n")
cat("Total columns:", ncol(ks4), "\n")
cat("Academic years included:", length(unique(ks4$time_period)), "\n")
cat("Years:", paste(sort(unique(ks4$time_period)), collapse = ", "), "\n")

# Show year distribution
year_counts <- table(ks4$time_period)
print(year_counts)

#### FURTHER MODIFY DATA ####


# gender

# # fill in number of boys/girls for all single sex schools
# ks4$num_ks4_boys <- ifelse(is.na(ks4$num_ks4_boys) & !is.na(ks4$num_ks4_girls), 0, ks4$num_ks4_boys)
# ks4$num_ks4_girls <- ifelse(is.na(ks4$num_ks4_girls) & !is.na(ks4$num_ks4_boys), 0, ks4$num_ks4_girls)
# sum(ks4$num_ks4_boys + ks4$num_ks4_girls != ks4$num_ks4, na.rm = T)

# Percentage of eligible boys/girls missing in 2010/11, recompute
ks4$perc_ks4_boys <- ifelse(is.na(ks4$perc_ks4_boys),
                            round(ks4$num_ks4_boys / ks4$num_ks4 * 100), ks4$perc_ks4_boys)
ks4$perc_ks4_girls <- ifelse(is.na(ks4$perc_ks4_girls), 
                             round(ks4$num_ks4_girls / ks4$num_ks4 * 100), ks4$perc_ks4_girls)

# # Check other percentages 
# 
# # prior attainment
# # respective level of prior attainment divided by the sum of pupils for which the information is available
# 
# denominator <- ks4$num_ks4_lo + ks4$num_ks4_mi + ks4$num_ks4_hi
# 
# levels <- c("lo", "mi", "hi")
# for (i in 1:length(levels)) {
#   numerator <- paste0("num_ks4_", levels[i])
#   quotient <- paste0("perc_ks4_", levels[i])
#   ks4[, quotient] <- ks4[, numerator] / denominator * 100
# }
# 
# # English language
# # respective category divided by number of KS4 pupils
# # note: sum(ks4$num_ks4_eal + ks4$num_ks4_efl + ks4$num_ks4_ufl != ks4$num_ks4, na.rm = T) retuns 0
# 
# denominator <- "num_ks4"
# 
# levels <- c("eal", "efl", "ufl")
# for (i in 1:length(levels)) {
#   numerator <- paste0("num_ks4_", levels[i])
#   quotient <- paste0("perc_ks4_", levels[i])
#   ks4[, quotient] <- ks4[, numerator] / ks4[, denominator] * 100
# }
# 
# # non-mobile pupils
# numerator <- "num_ks4_nmob"
# quotient <- "perc_ks4_nmob" 
# ks4[, quotient] <- ks4[, numerator] / ks4[, denominator] * 100

# Special educational needs

# Years	              Highest level                   Middle/Lower level                Grouped variables                   Transition variables
#   2010/11–2013/14   num_ks4_sen_st            	    num_ks4_sen_ap                    num_ks4_sen_aps
#   2014/15	          num_ks4_sen_st, num_ks4_sen_e   num_ks4_sen_ap, num_ks4_sen_k	    num_ks4_sen_aps, num_ks4_sen_se     num_ks4_sen_apk 
#   2015/16–2018/19   num_ks4_sen_se	                num_ks4_sen_apk	
#   2021/22 onwards 	num_ks4_sen_e                   num_ks4_sen_k

# ks4$num_ks4_sen_aps = ks4$num_ks4_sen_ap + ks4$num_ks4_sen_st
sum(ks4$num_ks4_sen_aps != ks4$num_ks4_sen_ap + ks4$num_ks4_sen_st, na.rm = T)
# ks4$num_ks4_sen_se = ks4$num_ks4_sen_e + ks4$num_ks4_sen_st
sum(ks4$num_ks4_sen_se != ks4$num_ks4_sen_e + ks4$num_ks4_sen_st, na.rm = T)
# in 2014/15: num_ks4_sen_apk = num_ks4_sen_k + num_ks4_sen_ap + unknown quantity
sum(ks4$num_ks4_sen_apk != ks4$num_ks4_sen_ap + ks4$num_ks4_sen_k, na.rm = T)

# all pupils on roll with special educational needs
ks4$num_ks4_sen_tot <- ifelse(ks4$time_period %in% c(201011, 201112, 201213, 201314), ks4$num_ks4_sen_aps,
                              ifelse(ks4$time_period %in% c(201415), ks4$num_ks4_sen_se + ks4$num_ks4_sen_apk,
                                     ifelse(ks4$time_period %in% c(201516, 201617, 201718, 201819), ks4$num_ks4_sen_se + ks4$num_ks4_sen_apk,
                                            ifelse(ks4$time_period %in% c(202122, 202223, 202324), ks4$num_ks4_sen_e + ks4$num_ks4_sen_k,
                                                   NA))))
sum(ks4$num_ks4_sen_tot != ks4$num_ks4_sen_all, na.rm = T) # check

# pupils on roll with highest level of need                   
ks4$num_ks4_sen_high <- ifelse(ks4$time_period %in% c(201011, 201112, 201213, 201314), ks4$num_ks4_sen_st,
                               ifelse(ks4$time_period %in% c(201415), ks4$num_ks4_sen_se,
                                      ifelse(ks4$time_period %in% c(201516, 201617, 201718, 201819), ks4$num_ks4_sen_se,
                                             ifelse(ks4$time_period %in% c(202122, 202223, 202324), ks4$num_ks4_sen_e,
                                                    NA))))
# pupils on roll with lower levels of need
ks4$num_ks4_sen_lower <- ifelse(ks4$time_period %in% c(201011, 201112, 201213, 201314), ks4$num_ks4_sen_ap,
                                ifelse(ks4$time_period %in% c(201415), ks4$num_ks4_sen_apk,
                                       ifelse(ks4$time_period %in% c(201516, 201617, 201718, 201819), ks4$num_ks4_sen_apk,
                                              ifelse(ks4$time_period %in% c(202122, 202223, 202324), ks4$num_ks4_sen_k,
                                                     NA))))
sum(ks4$num_ks4_sen_tot != ks4$num_ks4_sen_high + ks4$num_ks4_sen_lower, na.rm = T) # check

                      
# compute percentages
levels <- c("tot", "high", "lower")
denominator <- "num_ks4"

for (i in 1:length(levels)) {
  numerator <- paste0("num_ks4_sen_", levels[i])
  quotient <- paste0("perc_ks4_sen_", levels[i])
  ks4[, quotient] <- round(ks4[, numerator] / ks4[, denominator] * 100, 1)
}

# re-order and drop columns
ks4 <- ks4 %>% 
  relocate(any_of(c(paste0("num_ks4_sen_", levels), paste0("perc_ks4_sen_", levels))), .after = perc_ks4_girls) %>%
  arrange(laestab, time_period)

# delete more granular SEN data
levels <- c("aps", "ap", "st", "se", "apk", "k", "e", "all")
ks4[, paste0("num_ks4_sen_", levels)] <- NULL
ks4[, paste0("perc_ks4_sen_", levels)] <- NULL



#### save data ####

gc()

# re-order columns
ks4 <- ks4 %>% 
  select(-c(la, estab)) %>%
  relocate(time_period, school, laestab, urn) %>%
  arrange(laestab, time_period)

# save file
data.table::fwrite(ks4, file = file.path(dir_data, "tmp_data_spt_ks4.csv"), row.names = F)


#### CREATE TIMESERIES DATA ####

# ===== BASIC IDENTIFIERS =====

var <- "measures"

t1 <- c(201011, 201112, 201213)
v1 <- "grades_ag" # GCSE grades A to G
t2 <- c(201314)
v2 <- "grades_ag_ptq" # Restriction on vocational qualifications: Only DfE-approved qualifications count in performance tables.
t3 <- c(201415, 201516)
v3 <- "grades_ag_ptq_ee" # Early entry policy: Only a pupil’s first attempt at a GCSE counts
t4 <- c(201617, 201718, 201819, 202122, 202223, 202324)
v4 <- "grades_91_ptq_ee" # GCSE grades 9 to 1

ks4[, var] <- ifelse(ks4[, "time_period"] %in% t1, v1, 
                     ifelse(ks4[, "time_period"] %in% t2, v2, 
                            ifelse(ks4[, "time_period"] %in% t3, v3, 
                                   ifelse(ks4[, "time_period"] %in% t4, v4, 
                                                 NA))))

# ===== ATTAINMENT MEASURES =====

# Continuous measure of attainment outcomes #

# Total sum of Attainment 8 scores 

# extracted to approximate the old capped average point score per pupil, using the Attainment 8 framework
# (totatt8eng + totatt8mat + totatt8ebac + totatt8openg) / number of att8 pupils
# number of att8 pupils = round(totatt8 / att8scr)

# compute number of pupils in att8 measure
ks4$num_att8 <- round(ks4$totatt8 / ks4$att8)

# approximate the old capped average point score per pupil, using the Attainment 8 framework

# note: TTAPScp uses a points scale where the highest grade (A*) is 58 points, so total scores are in the hundreds, max possible 464
# Att8scr uses a simple 9–1 scale, so scores are much lower, max possible 72

ks4$caps8_approx <- (ks4$totatt8eng + ks4$totatt8mat + ks4$totatt8ebac + ks4$totatt8open) / ks4$num_att8
ks4$caps8gcse_approx <- (ks4$totatt8eng + ks4$totatt8mat + ks4$totatt8ebac + ks4$totatt8openg) / ks4$num_att8

# create tag
var <- "tag_caps"

t1 <- c(201011, 201112, 201213, 201314, 201415)
v1 <- "orig"
t2 <- c(201516, 201617, 201718, 201819, 202122, 202223, 202324)
v2 <- "approx"

ks4[, var] <- ifelse(ks4[, "time_period"] %in% t1, v1, 
                     ifelse(ks4[, "time_period"] %in% t2, v2, 
                                          NA))


# Key Stage 4 Performance Tables Column Lookup Table
# Organized by measurement type and time period (2010/11 - 2023/24)
# Contains standardized variable names mapped to original column variations

column_lookup_ks4 <- tibble(
  standard_name = c(
    # ===== BASIC IDENTIFIERS =====
    "time_period", "school", "laestab", "urn", "urn_ks4", "measures",
    
    # ===== PUPIL COUNTS AND DEMOGRAPHICS =====
    "num_ks4",
    "num_ks4_boys", "num_ks4_girls", 
    "perc_ks4_boys", "perc_ks4_girls", 
    
    # ===== OVERALL PERFORMANCE MEASURES =====
    # 2010/11-2014/15: Average point scores and level measures
    # "overall_aps",                                        # Total Average Point Score
    # "overall_avg_level",                                  # Average level per pupil (2012/13-2014/15)
    
    # ===== KEY STAGE 1 PRIOR ATTAINMENT =====
    
    "avg_ks2",
    "num_ks4_lo", "perc_ks4_lo",
    "num_ks4_mi", "perc_ks4_mi", 
    "num_ks4_hi", "perc_ks4_hi",
    
    # 2023 cohort was the last to have statutory KS1 assessment data available for progress calculations. 
    # From 2024, the DfE has shifted to using the Reception Baseline Assessment (RBA) 
    # as the starting point for measuring progress through primary school. 
    # RBA data will not be used for published progress measures until the first relevant cohort reaches the end of KS2 in 2028.
    
    # ===== LANGUAGE CHARACTERISTICS =====
    "num_ks4_eal", "perc_ks4_eal",        # English as Additional Language
    "num_ks4_efl", "perc_ks4_efl",        # English as First Language
    "num_ks4_ufl", "perc_ks4_ufl",        # Unclassified First Language
    "num_ks4_nmob", "perc_ks4_nmob",      # Non-mobile pupils, not availabe in 2010/11
    
    # ===== SEN MEASURES =====
    
    "num_ks4_sen_tot", "perc_ks4_sen_tot",
    "num_ks4_sen_high", "perc_ks4_sen_high",
    "num_ks4_sen_lower", "perc_ks4_sen_lower",
    
    # ===== DISADVANTAGE MEASURES =====
    "num_ks4_disadv", "perc_ks4_disadv",
    "num_ks4_notdisadv", "perc_ks4_notdisadv",
    
    # ===== ATTAINMENT MEASURES =====
    
    ## GCSE - General Certificate of Secondary Education ##
    
    # Overall good set of GCSE results
    "gcse5em",
    
    # GCSE results for English AND maths
    "gcse_em_std",
    "gcse_em_strong",
    
    ## English Baccalaureate (EBacc) ##
    
    # Entry
    
    "ebacc_e", # Percentage of Key Stage 4 pupils entering all English Baccalaureate
    "ebacc_e_lo", "ebacc_e_mi", "ebacc_e_hi",
    "ebacc_e_disadv", "ebacc_e_notdisadv",
    
    # Achievement
    
    "ebacc_std",
    "ebacc_std_disadv", 
    "ebacc_std_notdisadv",
    
    "ebacc_strong",
    "ebacc_strong_disadv", 
    "ebacc_strong_notdisadv",
    
    
    # after 2017/18
    "aps_ebacc", # Average EBacc APS score per pupil
    "aps_ebacc_boys", "aps_ebacc_girls",
    "aps_ebacc_lo", "aps_ebacc_mi", "aps_ebacc_hi",
    "aps_ebacc_eal",
    "aps_ebacc_disadv", "aps_ebacc_notdisadv",
    
    
    ## Total average (capped) point score per pupil ##
    
    "caps8",
    "caps8gcse",
    "tag_caps",
    
    ## Attainment 8 - from 2014/15 onwards ## 
    
    # Average Attainment 8 score per pupil
    "att8", 
    "att8_boys", "att8_girls",
    "att8_lo", "att8_mi", "att8_hi",
    "att8_eal",
    "att8_disadv", "att8_notdisadv",
    "att8_gap_nat",
    
    # Average score for different subjects
    
    "att8eng",	# Average Attainment 8 score per pupil for English element
    "att8eng_disadv", "att8eng_notdisadv",
    
    "att8mat",	# Average Attainment 8 score per pupil for mathematics element
    "att8mat_disadv", "att8mat_notdisadv",
    
    ## Progress 8 - from 2014/15 onwards ## 
    
    # Average Progress 8 score per pupil
    "prog8",
    "prog8_cov",
    "prog8_boys", "prog8_girls",
    "prog8_lo", "prog8_mi", "prog8_hi",
    "prog8_eal",
    "prog8_disadv", "prog8_notdisadv",
    "prog8_gap_nat",
    
    # Average score for different subjects
    
    "prog8eng",	# Progress 8 measure for English element
    "prog8eng_disadv", "prog8eng_notdisadv",
    
    "prog8mat",	# Progress 8 measure for mathematics element
    "prog8mat_disadv", "prog8mat_notdisadv"
    
  ),
  
  # Variations mapped to standard names
  variations = list(
    
    # Basic identifiers
    c("time_period"),
    c("school"),
    c("laestab"),
    c("urn"),
    c("urn_ks4"),
    c("measures"),
    
    # Pupil counts
    c("num_ks4"), # Number of pupils at the end of Key Stage 4
    c("num_ks4_boys"), # Number of boys at the end of key stage 4
    c("num_ks4_girls"), # Number of girls at the end of key stage 4
    c("perc_ks4_boys"),
    c("perc_ks4_girls"),
    
    # KS1 prior attainment
    c("ks2_aps", "ks2_ass"),
    c("num_ks4_lo"),
    c("perc_ks4_lo"),
    c("num_ks4_mi"),
    c("perc_ks4_mi"),
    c("num_ks4_hi"),
    c("perc_ks4_hi"),
    
    # Language characteristics
    c("num_ks4_eal"),
    c("perc_ks4_eal"),
    c("num_ks4_efl"),
    c("perc_ks4_efl"),
    c("num_ks4_ufl"),
    c("perc_ks4_ufl"),
    c("num_ks4_nmob"),
    c("perc_ks4_nmob"),
    
    # SEN measures
    c("num_ks4_sen_tot"), c("perc_ks4_sen_tot"),
    c("num_ks4_sen_high"), c("perc_ks4_sen_high"),
    c("num_ks4_sen_lower"), c("perc_ks4_sen_lower"),
    
    # Disadvantage measures (pre-2014/15 and 2014-15+)
    c("num_ks4_fsmcla", "num_ks4_fsmcla1a"),
    c("perc_ks4_fsmcla", "perc_ks4_fsmcla1a"),
    c("num_ks4_not_fsmcla", "num_ks4_not_fsmcla1a"),
    c("perc_ks4_not_fsmcla", "perc_ks4_not_fsmcla1a"),
    
    
    # ===== ATTAINMENT MEASURES =====
    
    ## GCSE - General Certificate of Secondary Education ##
    
    # Overall good set of GCSE results
    c("gcse5em_ac", "gcse5em_94"),
    
    # GCSE results for English AND maths
    
    c("em_ac", "em_94"),
    c("em_95"),
    
    ## English Baccalaureate (EBacc) ##
    
    # Entry
    
    c("ebacc_e"), # Percentage of Key Stage 4 pupils entering all English Baccalaureate
    c("ebacc_e_lo"), c("ebacc_e_mi"), c("ebacc_e_hi"),
    c("ebacc_e_disadv"), c("ebacc_e_notdisadv"),
    
    # Achievement
    
    c("ebacc", "ebacc_94"),
    c("ebacc_disadv", "ebacc_94_disadv"),
    c("ebacc_notdisadv", "ebacc_94_notdisadv"),
    
    c("ebacc_95"),
    c("ebacc_95_disadv"),
    c("ebacc_95_notdisadv"),
    
    c("aps_ebacc"), # Average EBacc APS score per pupil
    c("aps_ebacc_boys"), c("aps_ebacc_girls"),
    c("aps_ebacc_lo"), c("aps_ebacc_mi"), c("aps_ebacc_hi"),
    c("aps_ebacc_eal"),
    c("aps_ebacc_disadv"), c("aps_ebacc_notdisadv"),
    
    
    ## capped average point score ##
    
    c("caps8", "caps8_approx"),
    c("caps8gcse", "caps8gcse_approx"),
    c("tag_caps"),
    
    ## Attainment 8 ## 
    
    # Average Attainment 8 score per pupil
    c("att8"), 
    c("att8_boys"), c("att8_girls"),
    c("att8_lo"), c("att8_mi"), c("att8_hi"),
    c("att8_eal"),
    c("att8_disadv"), c("att8_notdisadv"),
    c("att8_gap_nat"),
    
    # Average score for different subjects
    
    c("att8eng"),	# Average Attainment 8 score per pupil for English element
    c("att8eng_disadv"), c("att8eng_notdisadv"),
    
    c("att8mat"),	# Average Attainment 8 score per pupil for mathematics element
    c("att8mat_disadv"), c("att8mat_notdisadv"),
    
    ## Progress 8 ## 
    
    # Average Progress 8 score per pupil
    c("prog8"),
    c("prog8_cov"),
    c("prog8_boys"), c("prog8_girls"),
    c("prog8_lo"), c("prog8_mi"), c("prog8_hi"),
    c("prog8_eal"),
    c("prog8_disadv"), c("prog8_notdisadv"),
    c("prog8_gap_nat"),
    
    # Average score for different subjects
    
    c("prog8eng"),	# Progress 8 measure for English element
    c("prog8eng_disadv"), c("prog8eng_notdisadv"),
    
    c("prog8mat"),	# Progress 8 measure for mathematics element
    c("prog8mat_disadv"), c("prog8mat_notdisadv")
    
  )
)

# Run the review of the column name lookup
review_lookup_mappings(lookup_table = column_lookup_ks4)

# Create the reverse lookup for ks4 data
reverse_lookup_ks4 <- create_reverse_lookup(column_lookup_ks4)

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
  tmp <- ks4[ks4$time_period == time_period, ]
  
  # Drop columns that are all NA
  tmp <- tmp[, !sapply(tmp, function(x) all(is.na(x)))]
  
  # **standardise COLUMN NAMES**
  tmp <- standardise_column_names(tmp, lookup = reverse_lookup_ks4)
  
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
column_lookup_ks4$standard_name[! column_lookup_ks4$standard_name %in% names(df)]

# re-order columns
df <- df[, column_lookup_ks4$standard_name]

#### save data ####

gc()

# re-order columns
df <- df %>% 
  arrange(laestab, time_period)

# save file
data.table::fwrite(df, file = file.path(dir_data, "data_spt_ks4.csv"), row.names = F)
