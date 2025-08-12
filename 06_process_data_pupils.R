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
names(spc)[names(spc) == "perc_pup_fsm"] <- "perc_pup_fsm_spc"
names(spc)[names(spc) == "num_pup_tot_fsm_calc"] <- "num_pup_tot_fsm_calc_spc"
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
names(spt)[names(spt) == "num_pup_tot_fsm_calc"] <- "num_pup_tot_fsm_calc_spt"
names(spt)[names(spt) == "num_pup_efl"] <- "num_spt_efl"
names(spt)[names(spt) == "num_pup_eal"] <- "num_spt_eal"
names(spt)[names(spt) == "num_pup_ufl"] <- "num_spt_ufl"

# Special educational needs in England
sen <- fread(file.path(dir_data, "data_sen.csv"))

# change column names
names(sen)[names(sen) == "num_pup_tot"] <- "num_sen_tot"
names(sen) <- gsub("num_pup_sen", "num_sen", names(sen))

# combine data.tables
df <- merge(spc, spt, by = id_cols, all = T)

# cols <- c(id_cols, "num_sen_tot", "num_sen", "num_sen_a", "num_sen_ap", "num_sen_st", "num_sen_k", "num_sen_e", "num_sen_se")
# df <- merge(df, sen[, ..cols], by = id_cols, all = T)
df <- merge(df, sen, by = id_cols, all = T)

df <- as.data.frame(df)

# possible variables to use
tmp <- df[, grepl("laestab|time|spc|spt|sen_tot", names(df))]
apply(tmp, 2, function(x){sum(is.na(x))})


df$num_sen_tot[is.na(df$num_spc_tot)]

# check if pupil numbers is the same in both datasets 
tmp <- tmp %>% 
  mutate(
    diff_pup = num_spc_tot - num_spt_tot,
    diff_pup2 = num_spc_tot - num_sen_tot,
    diff_pup3 = num_spt_tot - num_sen_tot,
    diff_boys = num_spc_boys - num_spt_boys,
    diff_girls = num_spc_girls - num_spt_girls,
    # diff_fsm = num_pup_fsm_spc - num_pup_fsm_spt,
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

# fill in missing obs using the SEN data
df[, col_tot] <- ifelse(is.na(df[, col_tot]), df$num_sen_tot, df[, col_tot])

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

# Total, boys and girls #

denominator <- "num_pup_tot"
levels <- c("tot", "boys", "girls")

for (i in 1:length(levels)) {
  
  spc <- paste0("num_spc_", levels[i])
  spt <- paste0("num_spt_", levels[i])
  
  numerator <- paste0("num_pup_", levels[i])
  quotient <- paste0("perc_pup_", levels[i])
  
  # fill NAs where possible
  df[, numerator] <- ifelse(is.na(df[, numerator]) & !is.na(df[, spt]), df[, spt], 
                            ifelse(is.na(df[, numerator]) & !is.na(df[, spc]), df[, spc], 
                                   df[, numerator]))
  
  if(numerator != denominator){
    
    # compute gender percentages
    df[, quotient] <- df[, numerator] / df[, denominator] * 100
    
  }
  
}

# Drop columns no longer needed
df[, paste0("num_spc_", levels)] <- NULL
df[, paste0("num_spt_", levels)] <- NULL



# Language categories #

levels <- c("efl", "eal", "ufl")

for (i in 1:length(levels)) {
  
  spc <- paste0("num_spc_", levels[i])
  spt <- paste0("num_spt_", levels[i])
  
  numerator <- paste0("num_pup_", levels[i])
  quotient <- paste0("perc_pup_", levels[i])

  # replace NAs in SPC data where possible
  df[, numerator] <- ifelse(is.na(df[, spc]) & !is.na(df[, spt]), df[, spt], df[, spc])
  
  # compute percentages
  df[, quotient] <- df[, numerator] / df[, denominator] * 100
  
  df <- df %>% 
    relocate(any_of(quotient), .after = any_of(numerator)) 
  
}

# Drop columns no longer needed
df[, paste0("num_spc_", levels)] <- NULL
df[, paste0("num_spt_", levels)] <- NULL

# FSM eligibility #

# create df with fsm relevant data only
check <- df[, grepl("urn|time|fsm", names(df))]
check$urn_sen <- NULL

# check data availability by year
avail <- check %>% group_by(time_period) %>%
  summarise(
    rows = sum(!is.na(urn_census)),
    
    num_pup_fsm_spc = sum(is.na(num_pup_fsm_spc)),
    perc_pup_fsm_spc = sum(is.na(perc_pup_fsm_spc)),
    num_pup_tot_fsm_calc_spc = sum(is.na(num_pup_tot_fsm_calc_spc)),    
    
    num_pup_fsm_performance_tables = sum(is.na(num_pup_fsm_performance_tables)),
    perc_pup_fsm_performance_tables = sum(is.na(perc_pup_fsm_performance_tables)),
    num_pup_tot_fsm_calc_performance_tables = sum(is.na(num_pup_tot_fsm_calc_performance_tables)),
    
    num_pup_fsm_spt = sum(is.na(num_pup_fsm_spt)), # 2010/11 - 2013/14, 2018/19, 2020/21 - 2023/24 
    perc_pup_fsm_spt = sum(is.na(perc_pup_fsm_spt)), # 2010/11 - 2017/18
    num_pup_tot_fsm_calc_spt = sum(is.na(num_pup_tot_fsm_calc_spt)) # 2010/11 - 2017/18
  )

# compare FSM numbers as reported in SPC and SPT (variables num_pup_fsm and perc_num_fsm) #
check$diff_num_pup_fsm <- check$num_pup_fsm_spc - check$num_pup_fsm_spt
check$diff_perc_pup_fsm <- check$perc_pup_fsm_spc - check$perc_pup_fsm_spt

# get descriptives of diff by year
# this suggests that in 2016/17 and 2018/19 - 2023/24, the same FSM numbers and/or percentages were reported in SPC and SPT census

# num_pup_fsm differs 2010/11 - 2013/14
# num_pup_fsm not reported in SPT from 2014/15 - 2017/18
# num_pup_fsm the same 2018/19 - 2023/24
psych::describeBy(check$diff_num_pup_fsm, group = check$time_period)

# perc_pup_fsm differs 2010/11 - 2015/16 and 2017/18
# perc_pup_fsm the same in 2016/17
# perc_pup_fsm not reported in SPT from 2018/19 - 2023/24
psych::describeBy(check$diff_perc_pup_fsm, group = check$time_period)

# compare FSM numbers from performance tables as reported in SPC and SPT #
check$diff_num_pup_fsm_performance_tables <- check$num_pup_fsm_performance_tables - check$num_pup_fsm_spt
check$diff_perc_pup_fsm_performance_tables <- check$perc_pup_fsm_performance_tables - check$perc_pup_fsm_spt

# get descriptives of diff by year
# this suggests that in 2010/11 - 2015/16 & 2017/18, the performance table results reported in the SPC data match the results in th SPT census 

# num_pup_fsm_performance_tables in SPC equal to num_pup_fsm in 2010/11 - 2013/14
# num_pup_fsm not reported in SPT from 2014/15 - 2017/18
# num_pup_fsm_performance_tables in SPC NOT equal to num_pup_fsm in 2018/19 - 2023/24
psych::describeBy(check$diff_num_pup_fsm_performance_tables, group = check$time_period)

# perc_pup_fsm_performance_tables in SPC equal to perc_pup_fsm in 2010/11 - 2015/16 & 2017/18
# perc_pup_fsm_performance_tables in SPC NOT equal to perc_pup_fsm in 2016/17
# perc_pup_fsm not reported in SPT from 2018/19 - 2023/24
psych::describeBy(check$diff_perc_pup_fsm_performance_tables, group = check$time_period)

# the data bove suggests that in 2016/17 and 2018/19 - 2023/24, the same FSM numbers and/or percentages (num_pup_fsm / perc_pup_fsm) were reported in SPC and SPT census
# hence, fill in any gaps in the data

t <- c(201617, 201819, 201920, 202021, 202122, 202223, 202324, 202425)

# num pup fsm
col <- "num_pup_fsm"
spc_col <- "num_pup_fsm_spc"
spt_col <- "num_pup_fsm_spt"
df[, col] <- ifelse(is.na(df[, spc_col]) & !is.na(df[, spt_col]) & df$time_period %in% t,
                       df[, spt_col], df[, spc_col])

# perc pup fsm
col <- "perc_pup_fsm"
spc_col <- "perc_pup_fsm_spc"
spt_col <- "perc_pup_fsm_spt"
df[, col] <- ifelse(is.na(df[, spc_col]) & !is.na(df[, spt_col]) & df$time_period %in% t,
                       df[, spt_col], df[, spc_col])

# compute percentages
# note: use total headcount of pupils as denominator for FSM calculations throughout
# This means the FSM percentage now reflects the proportion of eligible pupils among all pupils on roll, regardless of whether FSM status was collected for every pupil
numerator <- "num_pup_fsm"
quotient <- "perc_pup_fsm"
df[, quotient] <- df[, numerator] / df[, denominator] * 100

# compute percentages
numerator <- "num_pup_fsm6"
quotient <- "perc_pup_fsm6"
df[, quotient] <- df[, numerator] / df[, denominator] * 100

# SEN #

# understand data availability
# spt census
round(sum(is.na(df$urn_census))/nrow(df) * 100, 2)
round(sum(is.na(df$num_pup_sen_tot))/nrow(df) * 100, 2)

# SEN
round(sum(is.na(df$urn_sen))/nrow(df) * 100, 2)
round(sum(is.na(df$num_sen))/nrow(df) * 100, 2)

# compare pupil numbers reported in SPT and SEN data #

# extract all sen cols
check <- df[, grepl("urn|time|num_pup_sen|num_sen", names(df))]
check$num_sen_tot <- NULL
check$urn_ud_pupils <- NULL

# compute difference between SEN totals in SPT and SEN data
check$diff_tot <- check$num_pup_sen_tot - check$num_sen

# summarise results
check %>% group_by(time_period) %>%
  summarise(
    #  difference between SEN totals in SPT and SEN data
    n_tot = sum(diff_tot != 0, na.rm = T),
    min_tot = min(diff_tot, na.rm = T),
    max_tot = max(diff_tot, na.rm = T)
    )


# SEN data in 2010/11 missing some fields for some schools reported in SPT
# in SEN: Low counts (pupil numbers of 1 or 2) have been suppressed and replaced by (x) (for School Action Plus and Statement)
# in SPT: School Action Plus and Statement grouped together as num_pup_sen_aps

# 1. fill in (suppression) gaps in SEN num_sen_a data using SPT num_pup_sen_a data
t = c(201011, 201112, 201213, 201314)
var_sen = "num_sen_a"
var_spt = "num_pup_sen_a"
df[, var_sen] <- ifelse(df$time_period %in% t & is.na(df[, var_sen]) | df$time_period %in% t & df[, var_sen] == 0, 
                        df[, var_spt], df[, var_sen])

# 2. fill in (suppression) gaps in SEN num_sen_ap data using SPT num_pup_sen_ap data
t = c(201011, 201213, 201314)
var_sen = "num_sen_ap"
var_spt = "num_pup_sen_ap"
df[, var_sen] <- ifelse(df$time_period %in% t & is.na(df[, var_sen]) | df$time_period %in% t & df[, var_sen] == 0, 
                        df[, var_spt], df[, var_sen])

# 3. fill in (suppression) gaps in SEN num_sen_st data using SPT num_pup_sen_st data
t = c(201011, 201213, 201314)
var_sen = "num_sen_st"
var_spt = "num_pup_sen_st"
df[, var_sen] <- ifelse(df$time_period %in% t & is.na(df[, var_sen]) | df$time_period %in% t & df[, var_sen] == 0, 
                        df[, var_spt], df[, var_sen])

# 3. fill in (suppression) gaps in SEN num_sen_st and num_sen_ap data using SPT num_pup_sen_st data
# this registers all num_pup_sen_aps as num_sen_st
t = c(201011, 201112)
var_sen = "num_sen_st"
var_spt = "num_pup_sen_aps"
df[, var_sen] <- ifelse(df$time_period %in% t & is.na(df[, var_sen]) & is.na(df[, "num_sen_ap"]), 
                        df[, var_spt], df[, var_sen])

# 4. Deduct num_sen_st where possible
t = c(201112)
var_sen = "num_sen_st"
var_spt = "num_pup_sen_aps"

df[, var_sen] <- ifelse(df$time_period %in% t & is.na(df[, var_sen]) & !is.na(df[, var_spt]) & !is.na(df[, "num_sen_ap"]),
                        df[, var_spt] - df[, "num_sen_ap"], df[, var_sen])

# 5. Deduct num_sen_ap where possible
t = c(201112)
var_sen = "num_sen_ap"
var_spt = "num_pup_sen_aps"

df[, var_sen] <- ifelse(df$time_period %in% t & is.na(df[, var_sen]) & !is.na(df[, var_spt]) & !is.na(df[, "num_sen_st"]),
                        df[, var_spt] - df[, "num_sen_st"], df[, var_sen])


# 6. fill in suppression gaps in SEN num_sen_se data using SPT num_pup_sen_e data
t = c(201415, 201516, 201617, 201718)
var_sen = "num_sen_se"
var_spt = "num_pup_sen_e"
df[, var_sen] <- ifelse(df$time_period %in% t & is.na(df[, var_sen]), 
                        df[, var_spt], df[, var_sen])

# 7. fill in suppression gaps in SEN num_sen_k data using SPT num_pup_sen_k data
t = c(201415, 201516, 201617, 201718)
var_sen = "num_sen_k"
var_spt = "num_pup_sen_k"
df[, var_sen] <- ifelse(df$time_period %in% t & is.na(df[, var_sen]), 
                        df[, var_spt], df[, var_sen])

# 8. Re-compute total of pupils with SEN in SEN
df[, "num_sen_tmp"] <- ifelse(!is.na(df$num_sen),
                           rowSums(df[, c("num_sen_a", # School Action 2010/11 - 2013/14
                                          "num_sen_ap", # School Action Plus 2010/11 - 2013/14
                                          "num_sen_st", # Statement 2010/11 - 2013/14
                                          "num_sen_k", # SEN support 2014/15 - 2024/25
                                          "num_sen_se", # Statement or EHC 2014/15 - 2018/19
                                          "num_sen_e" # EHC 2014/15 - 2024/25 (until 2018/19, also included Statement)
                           )], na.rm = T),
                           df$num_sen)




# in 2015/16 - 2017/18, SPT data does not contain all sen_k data that the SEN data contains
# in 2016/17 - 2017/18  SPT data does not contain all sen_e data that the SEN data contains
# but SEN data 2012/13 to 2016/17 is rounded, so use SPT data where possible

# 1. fill in gaps in SPT num_pup_sen_k data using SEN num_sen_k data
t = c(201516, 201617, 201718)
var_sen = "num_sen_k"
var_spt = "num_pup_sen_k"
df[, var_spt] <- ifelse(df$time_period %in% t & is.na(df[, var_spt]) | df$time_period %in% t & df[, var_spt] == 0, 
                        df[, var_sen], df[, var_spt])
 
# 2. fill in gaps in SPT num_pup_sen_e data using SEN num_sen_se data
t = c(201617, 201718)
var_sen = "num_sen_e"
var_spt = "num_pup_sen_e"
df[, var_spt] <- ifelse(df$time_period %in% t & is.na(df[, var_spt]) | df$time_period %in% t & df[, var_spt] == 0, 
                        df[, var_sen], df[, var_spt])

# 3. Re-compute total of pupils with SEN in SPT
df[, "num_pup_sen_tot_tmp"] <- ifelse(!is.na(df$num_pup_sen_tot),
                                   rowSums(df[, c("num_pup_sen_a", # School Action 2010/11 - 2013/14
                                                  "num_pup_sen_aps", # School Action Plus or Statement 2010/11 - 2013/14
                                                  "num_pup_sen_k", # SEN support 2014/15 - 2024/25
                                                  "num_pup_sen_e" # EHC 2014/15 - 2024/25 (until 2018/19, also included Statement)
                                   )], na.rm = T),
                                   df$num_pup_sen_tot)

# extract all sen cols
check <- df[, grepl("urn|time|num_pup_sen|num_sen", names(df))]
check$num_sen_tot <- NULL
check$urn_ud_pupils <- NULL

# 2012/13 -  2016/17 data in SEN rounded, but not rounded in SPT

t = c(201213, 201314, 201415, 201516, 201617)

# function to round to nearest integer
mround <- function(x,base){
  base*round(x/base)
}

# round values in SPT
check[check$time_period %in% t, c("num_pup_sen_a", "num_pup_sen_ap", "num_pup_sen_st", "num_pup_sen_k", "num_pup_sen_e")] <- 
  apply(check[check$time_period %in% t, c("num_pup_sen_a", "num_pup_sen_ap", "num_pup_sen_st", "num_pup_sen_k", "num_pup_sen_e")], 2, 
        function(x){mround(x, base = 5)})

# re-compute rounded
check[, "num_pup_sen_tot_rd"] <- ifelse(check$time_period %in% t,
                                   rowSums(check[, c("num_pup_sen_a", # School Action 2010/11 - 2013/14
                                                  "num_pup_sen_ap", # School Action Plus 2010/11 - 2013/14
                                                  "num_pup_sen_st", # Statement 2010/11 - 2013/14
                                                  "num_pup_sen_k", # SEN support 2014/15 - 2024/25
                                                  "num_pup_sen_e" # EHC 2014/15 - 2024/25 (until 2018/19, also included Statement)
                                   )], na.rm = T),
                                   NA)

# compute difference between SEN totals in SPT and SEN data
check$diff_tot <- check$num_pup_sen_tot_tmp - check$num_sen_tmp

# compute difference between SEN totals in SPT and SEN data
check$diff_rd <- ifelse(check$time_period %in% t & !is.na(df$num_pup_sen_tot),
                        check$num_pup_sen_tot_rd - check$num_sen,
                        NA)

# summarise results
check %>% group_by(time_period) %>%
  summarise(
    #  difference between SEN totals in SPT and SEN data - all inconsistencies fixed
    n_tot = sum(diff_tot != 0, na.rm = T),
    min_tot = min(diff_tot, na.rm = T),
    max_tot = max(diff_tot, na.rm = T),
    
    #  difference between SEN totals in SPT and SEN data - rounded SPT data
    n_rd = sum(diff_rd != 0, na.rm = T),
    min_rd = min(diff_rd, na.rm = T),
    max_rd = max(diff_rd, na.rm = T))

# create final variable capturing the number of pupils with SEN
numerator <- "num_pup_sen"
quotient <- "perc_pup_sen"

df[, numerator] <- ifelse(is.na(df$num_pup_sen_tot_tmp), df$num_sen_tmp, df$num_pup_sen_tot_tmp)

# compute percentages
df[, quotient] <- df[, numerator] / df[, denominator] * 100


# delete other SPC sen cols
levels <- c("a", "ap", "st", "aps", "k", "e", "tot", "tot_tmp")
df[, paste0("num_pup_sen_", levels)] <- NULL
df[, paste0("perc_pup_sen_", levels)] <- NULL

# delete SEN sen cols
df[, grepl("num_sen", names(df))] <- NULL

# Ethnic origins of pupils #

levels <- c("white_british", "unclassified", "black", "asian", "mixed")

# compute percentages
for (i in 1:length(levels)) {
  numerator <- paste0("num_pup_ethn_", levels[i])
  quotient <- paste0("perc_pup_ethn_", levels[i])
  df[, quotient] <- df[, numerator] / df[, denominator] * 100
  
  df <- df %>% 
    relocate(any_of(quotient), .after = any_of(numerator)) 
}


# drop columns after fixing the roundings
df <- df[, !grepl("_spc_|_spt_|calc|table|urn_", names(df))]
df <- df[, !grepl("spc|spt|calc|table", names(df))]

# re-order columns
df <- df %>% 
  relocate(num_pup_tot, num_pup_boys, perc_pup_boys, num_pup_girls, perc_pup_girls, .after = school) %>%
  relocate(num_pup_fsm, perc_pup_fsm, .before = num_pup_fsm6) %>%
  relocate(fte_pupils, boarders_total, boarders_girls, boarders_boys, 
                    num_pup_ey, num_pup_nurs, num_pup_recep, num_pup_ks1, num_pup_ks2, num_pup_ks3, num_pup_ks4, num_pup_ks5, .after = last_col()) %>%
  arrange(laestab, time_period)

# remove all rows that only contain zeros or NAs
df$rowsum <- rowSums(df[!grepl("urn|laestab|school|time_period", names(df))], na.rm = T)
df <- df[df$rowsum != 0, ]
df$rowsum <- NULL

# save file
fwrite(df, file = file.path(dir_data, "data_pupils.csv"), row.names = F)


# apply(df, 2, FUN = function(x){sum(is.na(x))})
