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

# script variable definition #

# determine year list (akin to other data sources)
years_list <- paste0(20, 10:23, 11:24)
lookup <- data.frame(time_period = as.numeric(years_list),
                     academic_year = as.numeric(substr(years_list, 1, 4)))

id_cols <- c("time_period", "laestab", "urn", "school")


#### read in previously created files ####

# schools, pupils and characteristics
spc <- fread(file.path(dir_data, "data_spc_pupils.csv")) # census data collected in January of academic year - Spring census

# change column names
names(spc)[names(spc) == "num_pup_total"] <- "num_spc_tot"
names(spc)[names(spc) == "num_pup_boys"] <- "num_spc_boys"
names(spc)[names(spc) == "num_pup_girls"] <- "num_spc_girls"
names(spc)[names(spc) == "num_pup_fsm"] <- "num_pup_fsm_spc"
names(spc)[names(spc) == "num_pup_efl"] <- "num_spc_efl"
names(spc)[names(spc) == "num_pup_eal"] <- "num_spc_eal"
names(spc)[names(spc) == "num_pup_ufl"] <- "num_spc_ufl"

# performance table census
spt <- fread(file.path(dir_data, "data_spt_census.csv"))

# change column names
names(spt)[names(spt) == "num_pup_tot"] <- "num_spt_tot"
names(spt)[names(spt) == "num_pup_boys"] <- "num_spt_boys"
names(spt)[names(spt) == "num_pup_girls"] <- "num_spt_girls"
names(spt)[names(spt) == "num_pup_fsm"] <- "num_pup_fsm_spt"
names(spt)[names(spt) == "perc_pup_fsm"] <- "perc_pup_fsm_spt"
names(spt)[names(spt) == "num_pup_efl"] <- "num_spt_efl"
names(spt)[names(spt) == "num_pup_eal"] <- "num_spt_eal"
names(spt)[names(spt) == "num_pup_ufl"] <- "num_spt_ufl"

# combine data.tables
df <- merge(spc, spt, by = id_cols, all.x = T)
df <- as.data.frame(df)

# possible variables to use
tmp <- df[, grepl("laestab|time|spc|spt", names(df))]
apply(tmp, 2, function(x){sum(is.na(x))})

# check if pupil numbers is the same in both datasets 
tmp <- tmp %>% 
  mutate(
    diff_pup = num_spc_tot - num_spt_tot,
    diff_boys = num_spc_boys - num_spt_boys,
    diff_girls = num_spc_girls - num_spt_girls,
    diff_fsm = num_pup_fsm_spc - num_pup_fsm_spt,
    diff_efl = num_spc_efl - num_spt_efl,
    diff_eal = num_spc_eal - num_spt_eal,
    diff_ufl = num_spc_ufl - num_spt_ufl,
    )

apply(tmp[, grepl("diff", names(tmp))], 2, psych::describe)

# all differences in pupil headcount between -2 and 2 --> caused by different roundings to the nearest 5er
tmp %>%
  group_by(time_period) %>%
  summarise(
    # count cases of mismatch
    count_pup = sum(diff_pup != 0, na.rm = T),
    count_boys = sum(diff_boys != 0, na.rm = T),
    count_girls = sum(diff_girls != 0, na.rm = T),
    range_pup_spc = paste0(min(num_spc_tot[!is.na(diff_pup)], na.rm = T), "; ", max(num_spc_tot[!is.na(diff_pup)], na.rm = T)),
    range_pup_spt = paste0(min(num_spt_tot[!is.na(diff_pup)], na.rm = T), "; ", max(num_spt_tot[!is.na(diff_pup)], na.rm = T)),
    range_boys_spc = paste0(min(num_spc_boys[!is.na(diff_boys)], na.rm = T), "; ", max(num_spc_boys[!is.na(diff_boys)], na.rm = T)),
    range_boys_spt = paste0(min(num_spt_boys[!is.na(diff_boys)], na.rm = T), "; ", max(num_spt_boys[!is.na(diff_boys)], na.rm = T)),
    range_girls_spc = paste0(min(num_spc_girls[!is.na(diff_girls)], na.rm = T), "; ", max(num_spc_girls[!is.na(diff_girls)], na.rm = T)),
    range_girls_spt = paste0(min(num_spt_girls[!is.na(diff_girls)], na.rm = T), "; ", max(num_spt_girls[!is.na(diff_girls)], na.rm = T))
  )

# total pupil headcount #

# total pupil headcount data differs in 2010/11 - 2012/13
# rounding applied to nearest 5 in total pupil headcount data collected in 2010/11 - 2012/13 in SPC
#   data in num_spc_tot is rounded to nearest 5, data in num_spt_tot is not rounded
#   NOTE: data on total number of pupils is the same for SPC and SPT tables once SPT data is rounded to the nearest 5 for the years 2010/11 - 2012/13 (!)
col_tot <- "num_pup_tot"

# fix roundings
fixed <- fix_roundings(var_rd = "num_spc_tot", var_nrd = "num_spt_tot",
                     new_var = col_tot,
                     identifier_columns = id_cols,
                     col_to_filter = "time_period",
                     filter = c(201011, 201112, 201213),
                     rounding_factor = 5,
                     data_in = df)

# add to df
df <- merge(df, fixed[, c(id_cols, col_tot)], by = id_cols, all.x = T)

# headcount boys and girls #

# pupil headcount data for boys and girls differs in 2011/12 - 2016/17         
# rounding applied to nearest 5 in pupil headcount data for boys and girls collected in 2011/12 - 2016/17 in SPC
#   data in num_spc_boys is rounded to nearest 5, data in num_spt_boys is not rounded
#   NOTE: data on total number of pupils is the same for SPC and SPT tables once SPT data is rounded to the nearest 5 for the years 2011/12 - 2016/17

# BOYS
col_tot <- "num_pup_boys"

# fix roundings
fixed <- fix_roundings(var_rd = "num_spc_boys", var_nrd = "num_spt_boys",
                     new_var = col_tot,
                     identifier_columns = id_cols,
                     col_to_filter = "time_period",
                     filter = c(201112, 201213, 201314, 201415, 201516, 201617),
                     rounding_factor = 5,
                     data_in = df)

# add to df
df <- merge(df, fixed[, c(id_cols, col_tot)], by = id_cols, all.x = T)

# GIRLS
col_tot <- "num_pup_girls"

# fix roundings
fixed <- fix_roundings(var_rd = "num_spc_girls", var_nrd = "num_spt_girls",
                     new_var = col_tot,
                     identifier_columns = id_cols,
                     col_to_filter = "time_period",
                     filter = c(201112, 201213, 201314, 201415, 201516, 201617),
                     rounding_factor = 5,
                     data_in = df)

# add to df
df <- merge(df, fixed[, c(id_cols, col_tot)], by = id_cols, all.x = T)


rm(tmp, fixed)
gc()

# fill NAs where possible #

# total
df$num_pup_tot <- ifelse(is.na(df$num_pup_tot) & !is.na(df$num_spt_tot), df$num_spt_tot, 
                          ifelse(is.na(df$num_pup_tot) & !is.na(df$num_spc_tot), df$num_spc_tot, 
                                 df$num_pup_tot))
# boys
df$num_pup_boys <- ifelse(is.na(df$num_pup_boys) & !is.na(df$num_spt_boys), df$num_spt_boys, 
                          ifelse(is.na(df$num_pup_boys) & !is.na(df$num_spc_boys), df$num_spc_boys, 
                                 df$num_pup_boys))
# girls
df$num_pup_girls <- ifelse(is.na(df$num_pup_girls) & !is.na(df$num_spt_girls), df$num_spt_girls, 
                          ifelse(is.na(df$num_pup_girls) & !is.na(df$num_spc_girls), df$num_spc_girls, 
                                 df$num_pup_girls))

# Gender percentages #
denominator <- "num_pup_tot"

numerator <- "num_pup_girls"
quotient <- "perc_pup_girls"
df[, quotient] <- df[, numerator] / df[, denominator] * 100

numerator <- "num_pup_boys"
quotient <- "perc_pup_boys"
df[, quotient] <- df[, numerator] / df[, denominator] * 100

# Language categories #

# no differences found
# replace NAs in SPC data where possible
df$num_pup_efl <- ifelse(is.na(df$num_spc_efl) & !is.na(df$num_spt_efl), df$num_spt_efl, df$num_spc_efl)
df$num_pup_eal <- ifelse(is.na(df$num_spc_eal) & !is.na(df$num_spt_eal), df$num_spt_eal, df$num_spc_eal)
df$num_pup_ufl <- ifelse(is.na(df$num_spc_ufl) & !is.na(df$num_spt_ufl), df$num_spt_ufl, df$num_spc_ufl)

# compute percentages
numerator <- "num_pup_efl"
quotient <- "perc_pup_efl"
df[, quotient] <- df[, numerator] / df[, denominator] * 100

numerator <- "num_pup_eal"
quotient <- "perc_pup_eal"
df[, quotient] <- df[, numerator] / df[, denominator] * 100

numerator <- "num_pup_ufl"
quotient <- "perc_pup_ufl"
df[, quotient] <- df[, numerator] / df[, denominator] * 100

# FSM eligibility #

# HUGE differences in fsm
# replace NAs in SPC data where possible
df$num_pup_fsm <- ifelse(is.na(df$num_pup_fsm_spc) & !is.na(df$num_pup_fsm_spt), df$num_pup_fsm_spt, df$num_pup_fsm_spc)

# compute percentages
numerator <- "num_pup_fsm"
quotient <- "perc_pup_fsm"
df[, quotient] <- df[, numerator] / df[, denominator] * 100

# compute percentages
numerator <- "num_pup_fsm6"
quotient <- "perc_pup_fsm6"
df[, quotient] <- df[, numerator] / df[, denominator] * 100

# SEN #

levels <- c("tot", "high", "lower")

# compute percentages
for (i in 1:length(levels)) {
  numerator <- paste0("num_pup_sen_", levels[i])
  quotient <- paste0("perc_pup_sen_", levels[i])
  df[, quotient] <- df[, numerator] / df[, denominator] * 100
}

# Ethnic origins of pupils #

levels <- c("white_british", "unclassified", "black", "asian", "mixed")

# compute percentages
for (i in 1:length(levels)) {
  numerator <- paste0("num_pup_ethn_", levels[i])
  quotient <- paste0("perc_pup_ethn_", levels[i])
  df[, quotient] <- df[, numerator] / df[, denominator] * 100
}


# drop columns after fixing the roundings
df <- df[, !grepl("_spc_|_spt_|calc|table", names(df))]
df <- df[, !grepl("spc|spt|calc|table", names(df))]

# re-order columns
df <- df %>% 
  relocate(num_pup_tot, num_pup_boys, num_pup_girls, perc_pup_boys, perc_pup_girls, .before = fte_pupils) %>%
  relocate(any_of(c(paste0("num_pup_ethn_", levels), paste0("perc_pup_ethn_", levels))), .after = last_col()) %>%
  arrange(laestab, time_period)

# remove all rows that only contain zeros or NAs
df$rowsum <- rowSums(df[!grepl("urn|laestab|school|time_period", names(df))], na.rm = T)
df <- df[df$rowsum != 0, ]
df$rowsum <- NULL

# save file
fwrite(df, file = file.path(dir_data, "data_pupils.csv"), row.names = F)


apply(df, 2, FUN = function(x){sum(is.na(x))})
