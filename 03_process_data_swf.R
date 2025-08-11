# School Workforce Census (SWC) #

# Data source: SWC 2024 (November).

options(scipen = 999)


# Empty the workspace
rm(list = ls())
gc()

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

# Define the base directory
dir <- get_directory()
dir_data <- file.path(dir, "data")
dir_misc <- file.path(dir, "misc")
dir_in <- file.path(dir_data, "school-workforce-in-england")


# load libraries
library(dplyr)
library(rlang)
library(data.table)

# Get Information about Schools #

# read in establishment data
gias <- as.data.frame(fread(file.path(dir_data, "data_gias_search.csv"), encoding = "UTF-8"))

# remove all establishments without an laestab (i.e., Children's centres, British schools overseas, Online providers)
gias <- gias[!is.na(gias$laestab), ]

# select relevant laestab and urn only and relevant columns
gias <- gias[!duplicated(gias), c("laestab", "urn", "establishmentname")]
names(gias) <- c("laestab", "urn_gias", "school")

get_urns <- F

# determine year list (akin to other data sources)
years_list <- paste0(20, 10:24, 11:25)
lookup <- data.frame(time_period = as.numeric(years_list),
                     academic_year = as.numeric(substr(years_list, 1, 4)))
id_cols <- c("time_period", "urn")
id_cols <- c("time_period", "urn", "laestab")
id_cols <- c("time_period", "urn", "laestab", "school")


# Pupil to teacher ratios - school level #

# read in data
ptr <- read.csv(file.path(dir_in, "2024", "data", "workforce_ptrs_2010_2024_sch.csv"))

ptr <- ptr %>%
  # rename columns 
  rename_with(., ~tolower(gsub("X...", "", ., fixed = T))) %>% 
  # remove columns that are uninformative
  select(where(~length(unique(na.omit(.x))) > 1)) %>%
  as.data.frame()


# select columns
ptr <- ptr[, grepl("time_period|urn|laestab|fte|ratio|school", names(ptr))]
ptr <- ptr[, !grepl("code|number|type", names(ptr))]

# check urns and clean up data
ptr <- cleanup_data(data_in = ptr)

# Teacher absences - school level #

# read in data
abs <- read.csv(file.path(dir_in, "2024", "data", "sickness_absence_teachers_sch.csv"))

abs <- abs %>%
  # rename columns 
  rename_with(., ~tolower(gsub("X...", "", ., fixed = T))) %>% 
  # remove columns that are uninformative
  select(where(~length(unique(na.omit(.x))) > 1)) %>%
  as.data.frame()

# select columns
abs <- abs[, grepl("time_period|urn|laestab|abs|day|school", names(abs))]
abs <- abs[, !grepl("code|type", names(abs))]

# check urns and clean up data
abs <- cleanup_data(data_in = abs)


# Teacher pay - school level #

# read in data
pay <- read.csv(file.path(dir_in, "2024", "data", "workforce_teacher_pay_2010_2024_school.csv"))

pay <- pay %>%
  # rename columns 
  rename_with(., ~tolower(gsub("X...", "", ., fixed = T))) %>% 
  # remove columns that are uninformative
  select(where(~length(unique(na.omit(.x))) > 1)) %>%
  as.data.frame()

# select columns
pay <- pay[, grepl("time_period|urn|laestab|average|headcount|pay|school", names(pay))]
pay <- pay[, !grepl("code|type", names(pay))]

# check urns and clean up data
pay <- cleanup_data(data_in = pay)

# Teacher vacancies - school level #

# read in data
vac <- read.csv(file.path(dir_in, "2024", "data", "vacancies_number_rate_sch_2010_2024.csv"))

vac <- vac %>%
  # rename columns 
  rename_with(., ~tolower(gsub("X...", "", ., fixed = T))) %>% 
  # remove columns that are uninformative
  select(where(~length(unique(na.omit(.x))) > 1)) %>%
  as.data.frame()

# select columns
vac <- vac[, grepl("time_period|urn|laestab|school|vac|rate|temp", names(vac))]
vac <- vac[, !grepl("code|type", names(vac))]

# check urns and clean up data
vac <- cleanup_data(data_in = vac)


# Teacher turnover - school level #

tto <- read.csv(file.path(dir_in, "2024", "data", "teacher_turnover_2010_2024_sch.csv"))

# fix variable names
names(tto) <- c("time_period", "time_identifier", "geographic_level", "country_code", "country_name",
                "school_urn", "school_laestab", "school_name", "school_type", 
                "teacher_fte_in_census_year", "remained_in_the_same_school", "left_the_state_funded_system", "left_to_another_state_funded_school")

tto <- tto %>%
  # rename columns 
  rename_with(., ~tolower(gsub("X...", "", ., fixed = T))) %>% 
  # remove columns that are uninformative
  select(where(~length(unique(na.omit(.x))) > 1)) %>%
  # compute percentage retained
  mutate(retention_rate = remained_in_the_same_school / teacher_fte_in_census_year * 100) %>%
  # drop columns
  # select(-c(school_type, teacher_fte_in_census_year, remained_in_the_same_school, left_the_state.funded_system, left_to_another_state.funded_school)) %>%
  select(-c(school_type)) %>%
  as.data.frame()
  
# check urns and clean up data
tto <- cleanup_data(data_in = tto)


# Size of the school workforce - school level #

# read in data
swf <- read.csv(file.path(dir_in, "2024", "data", "workforce_2010_2024_fte_hc_nat_reg_la_sch.csv"))

swf <- swf %>%
  # rename columns 
  rename_with(., ~tolower(gsub("X...", "", ., fixed = T))) %>% 
  # remove columns that are uninformative
  select(where(~length(unique(na.omit(.x))) > 1)) %>%
  as.data.frame()

# select columns
swf <- swf[, !grepl("fte_ft|fte_pt|hc_pt|hc_ft|code|type|region|la_", names(swf))]

# check urns and clean up data
swf <- cleanup_data(data_in = swf)




# Workforce teacher characteristics - school level #

pattern_wtc <- "workforce_teacher_characteristics_school_201011_202425"
dir_wtc <- file.path(dir_in, "2024", "supporting-files")
dir_tmp <- file.path(dir_wtc, pattern_wtc)

"data/school-workforce-in-england/2024/supporting-files/workforce_teacher_characteristics_school_201011_202425.zip"
unzip = F
if (unzip) {
  # determine zipped folder
  zipped_folder <- list.files(path = dir_wtc, pattern = pattern_wtc, full.names = T)
  
  # create output folder
  dir.create(dir_tmp)
  
  # unzip
  unzip(zipped_folder, exdir = dir_tmp)
  
}

gc()

# create vector with file names
files <- list.files(path = dir_tmp, full.names = T)
files <- files[!grepl("meta", files)]

# determine cols to keep
cols_to_keep <- c("time_period", "school_urn", "school_laestab", "school_name",
                  "characteristic_group", "characteristic",
                  "gender", "sex", "age_group", "ethnicity_major",
                  "grade", "working_pattern", "qts_status", "on_route",
                  "full_time_equivalent", "headcount", "fte_school_percent", "headcount_school_percent")

for (f in 1:length(files)) {
  
  # read in data
  tmp <- read.csv(file = files[f])
  names(tmp) <- tolower(gsub("X...", "", names(tmp), fixed = T))
  
  # select relevant columns
  tmp <- tmp[, names(tmp) %in% cols_to_keep]
  
  # combine across years
  if (f == 1) {
    teach_char <- tmp
  } else {
    teach_char <- rbind.all.columns(teach_char, tmp)
  }
  
}

# check urns and clean up data
id_lookup <- create_urn_laestab_lookup(data_in = teach_char)
id_lookup <- id_lookup$lookup

# fix id information in input data
teach_char <- teach_char %>% 
  # add correct ids
  full_join(id_lookup, .) %>%
  # rename column
  rename(urn_wtc = school_urn) %>%
  # drop school_name and school_laestab
  select(-c(school_laestab, school_name)) %>%
  # sort data
  arrange(laestab, time_period) %>%
  as.data.frame()
rm(id_lookup)
gc()

# rename columns
names(teach_char) <- gsub("headcount", "hc", names(teach_char))
names(teach_char) <- gsub("school_percent", "perc", names(teach_char))
names(teach_char) <- gsub("full_time_equivalent", "fte", names(teach_char))



# replace values 
characteristics <- c("grade", "sex", "age_group", 
                     "working_pattern", "qts_status", "on_route", "ethnicity_major")

teach_char <- teach_char %>%
  mutate(across(matches("fte|hc"), ~na_if(., "x"))) %>% # x = not available - information has not been collected or there are no estimates available at this level of aggregation.
  mutate(across(matches("fte|hc"), ~na_if(., "z"))) %>% # z = not applicable - statistic cannot be produced. For example where a denominator is not available to produce a percentage.
  mutate(across(matches("fte|hc"), ~na_if(., "c"))) %>% # c = confidential - where presentation of data would disclose confidential information
  mutate(across(matches("fte|hc"), ~na_if(., "u"))) %>% # u = low reliability - values of the potentially low quality, for example where values of statistical significance have been calculated.
  # remove the comma and then convert the resulting string to a numeric type
  mutate(across(matches("fte|hc"), ~as.numeric(gsub(",", "", .)))) %>%
  # replace spaces
  mutate(across(all_of(characteristics), ~gsub(" ", "_", .))) %>%
  # Add characteristic_group == "Total" & characteristic == "Total" for years where the information is not included
  mutate(check = rowSums(teach_char[, characteristics] != "Total")) %>%
  mutate(characteristic_group = ifelse(is.na(characteristic_group) & check == 0, "Total", characteristic_group)) %>%
  mutate(characteristic = ifelse(is.na(characteristic) & check == 0, "Total", characteristic)) %>%
  select(-check) %>%
  # re-compute the percentages 
  # determine total
  mutate(
    hc_total = first(hc[characteristic_group == "Total" & characteristic == "Total"]),
    fte_total = first(fte[characteristic_group == "Total" & characteristic == "Total"]),
    .by = c(time_period, urn)
  ) %>%
  # re-compute percentages
  mutate(
    hc_perc = hc / hc_total * 100,
    fte_perc = fte / fte_total * 100
  ) %>%
  # drop total columns
  select(-c(hc_total, fte_total)) %>%
  as.data.frame()
gc()

# determine value variables
values <- c("hc", "fte", "hc_perc", "fte_perc")
# values <- c("hc")

cols <- c(id_cols, "urn_wtc")


# make into wide format
wtc <- teach_char %>% 
  # TOTALS
  filter_at(vars(characteristics), all_vars(. == "Total")) %>%
  select(all_of(c(cols, "hc", "fte"))) %>%
  right_join( # SEX
    teach_char %>%
      # filter(sex != "Total", sex != "sex_Unclassified") %>%
      filter(sex != "Total") %>%
      tidyr::pivot_wider(id_cols = {cols},
                         names_from = sex,
                         names_glue = "{.value}_sex_{sex}",
                         values_from = {values}, values_fill = 0),
    by = cols) %>%
  right_join( # AGE
    teach_char %>% 
      # filter(age_group != "Total", age_group != "Age_unclassified") %>%
      filter(age_group != "Total") %>%
      tidyr::pivot_wider(id_cols = {cols},
                         names_from = age_group,
                         names_glue = "{.value}_age_{age_group}",
                         values_from = {values}, values_fill = 0),
    by = cols) %>%
  right_join( # ETHNICITY
    teach_char %>% 
      # filter(ethnicity_major != "Total", ethnicity_major != "Information_not_yet_obtained", ethnicity_major != "Refused") %>%
      filter(ethnicity_major != "Total") %>%
      tidyr::pivot_wider(id_cols = {cols},
                         names_from = ethnicity_major,
                         names_glue = "{.value}_ethnicity_{ethnicity_major}",
                         values_from = {values}, values_fill = 0),
    by = cols) %>%
  right_join( # GRADE
    teach_char %>% 
      filter(grade != "Total") %>%
      tidyr::pivot_wider(id_cols = {cols},
                         names_from = grade,
                         names_glue = "{.value}_grade_{grade}",
                         values_from = {values}, values_fill = 0),
    by = cols) %>%
  right_join( # WORKING PATTERN
    teach_char %>% 
      filter(working_pattern != "Total") %>%
      tidyr::pivot_wider(id_cols = {cols},
                         names_from = working_pattern,
                         names_glue = "{.value}_pattern_{working_pattern}",
                         values_from = {values}, values_fill = 0),
    by = cols) %>%
  right_join( # QTS STATUS
    teach_char %>% 
      filter(qts_status != "Total") %>%
      tidyr::pivot_wider(id_cols = {cols},
                         names_from = qts_status,
                         names_glue = "{.value}_qts_{qts_status}",
                         values_from = {values}, values_fill = 0),
    by = cols)

# MUTATE data
wtc <- wtc %>%
  mutate(
    # fill gaps in total data: grade chosen here but could have also used age, ethnicity, pattern or qts
    tmp_hc = rowSums(across(matches("hc_grade")), na.rm = T),
    hc = ifelse(is.na(hc), tmp_hc, hc),
    tmp_fte = rowSums(across(matches("fte_grade")), na.rm = T),
    fte = ifelse(is.na(fte), tmp_fte, fte)
    ,
    # fill NAs with zeros where possible: GENDER
    # tmp = get HC count based on all gender columns
    # if count is equal to HC (tmp == hc), then replace NA with 0 in all gender columns
    tmp = rowSums(across(matches("hc_gender_")), na.rm = T),
    across(matches("hc_gender_"), ~ifelse(tmp == hc & is.na(.), 0, .)),
    across(matches("fte_gender_"), ~ifelse(tmp == hc & is.na(.), 0, .)),
    across(matches("hc_perc_gender_"), ~ifelse(tmp == hc & is.na(.), 0, .)),
    across(matches("fte_perc_gender_"), ~ifelse(tmp == hc & is.na(.), 0, .))
    ,
    # fill NAs with zeros where possible: AGE
    # tmp = get HC count based on all age columns
    # if count is equal to HC (tmp == hc), then replace NA with 0 in all age columns
    tmp = rowSums(across(matches("hc_age_")), na.rm = T),
    across(matches("hc_age_"), ~ifelse(tmp == hc & is.na(.), 0, .)),
    across(matches("fte_age_"), ~ifelse(tmp == hc & is.na(.), 0, .)),
    across(matches("hc_perc_age_"), ~ifelse(tmp == hc & is.na(.), 0, .)),
    across(matches("fte_perc_age_"), ~ifelse(tmp == hc & is.na(.), 0, .))
    ,
    # fill NAs with zeros where possible: ETHNICITY
    # tmp = get HC count based on all ethnicity columns
    # if count is equal to HC (tmp == hc), then replace NA with 0 in all ethnicity columns
    tmp = rowSums(across(matches("hc_ethnicity_")), na.rm = T),
    across(matches("hc_ethnicity_"), ~ifelse(tmp == hc & is.na(.), 0, .)),
    across(matches("fte_ethnicity_"), ~ifelse(tmp == hc & is.na(.), 0, .)),
    across(matches("hc_perc_ethnicity_"), ~ifelse(tmp == hc & is.na(.), 0, .)),
    across(matches("fte_perc_ethnicity_"), ~ifelse(tmp == hc & is.na(.), 0, .))
    ,
    # fill NAs with zeros where possible: GRADE
    # tmp = get HC count based on all grade columns
    # if count is equal to HC (tmp == hc), then replace NA with 0 in all grade columns
    tmp = rowSums(across(matches("hc_grade_")), na.rm = T),
    across(matches("hc_grade_"), ~ifelse(tmp == hc & is.na(.), 0, .)),
    across(matches("fte_grade_"), ~ifelse(tmp == hc & is.na(.), 0, .)),
    across(matches("hc_perc_grade_"), ~ifelse(tmp == hc & is.na(.), 0, .)),
    across(matches("fte_perc_grade_"), ~ifelse(tmp == hc & is.na(.), 0, .))
    ,
    # fill NAs with zeros where possible: PATTERN
    # tmp = get HC count based on all pattern columns
    # if count is equal to HC (tmp == hc), then replace NA with 0 in all pattern columns
    tmp = rowSums(across(matches("hc_pattern_")), na.rm = T),
    across(matches("hc_pattern_"), ~ifelse(tmp == hc & is.na(.), 0, .)),
    across(matches("fte_pattern_"), ~ifelse(tmp == hc & is.na(.), 0, .)),
    across(matches("hc_perc_pattern_"), ~ifelse(tmp == hc & is.na(.), 0, .)),
    across(matches("fte_perc_pattern_"), ~ifelse(tmp == hc & is.na(.), 0, .))
    ,
    # fill NAs with zeros where possible: QTS
    # tmp = get HC count based on all qts columns
    # if count is equal to HC (tmp == hc), then replace NA with 0 in all qts columns
    tmp = rowSums(across(matches("hc_qts_")), na.rm = T),
    across(matches("hc_qts_"), ~ifelse(tmp == hc & is.na(.), 0, .)),
    across(matches("fte_qts_"), ~ifelse(tmp == hc & is.na(.), 0, .)),
    across(matches("hc_perc_qts_"), ~ifelse(tmp == hc & is.na(.), 0, .)),
    across(matches("fte_perc_qts_"), ~ifelse(tmp == hc & is.na(.), 0, .))
    ,
    # delete tmp
    tmp = NULL,
    tmp_fte = NULL,
    tmp_hc = NULL)

# CHECK NA COUNT: zero for all but gender
na_count <- as.data.frame(apply(wtc, 2, function(x){sum(is.na(x))}))
na_count$var = row.names(na_count)
rm(na_count)
gc()

# COMPUTE GROUP AGGREGRATES
wtc <- wtc %>%
  mutate(
    # COMPUTE LARGER AGE GROUPS: UNDER 30
    # add up all groups that are combined in the larger group and divide by TOTAL hc or fte
    hc_age_under_30 = rowSums(select(., "hc_age_Under_25", "hc_age_25_to_29"), na.rm = T),
    hc_perc_age_under_30 = hc_age_under_30/hc * 100,
    fte_age_under_30 = rowSums(select(., "fte_age_Under_25", "fte_age_25_to_29"), na.rm = T),
    fte_perc_age_under_30 = fte_age_under_30/fte * 100,
    
    # COMPUTE LARGER AGE GROUPS: 30 - 49
    # add up all groups that are combined in the larger group and divide by TOTAL hc or fte
    hc_age_30_to_49 = rowSums(select(., "hc_age_30_to_39", "hc_age_40_to_49"), na.rm = T),
    hc_perc_age_30_to_49 = hc_age_30_to_49/hc * 100,
    fte_age_30_to_49 = rowSums(select(., "fte_age_30_to_39", "fte_age_40_to_49"), na.rm = T),
    fte_perc_age_30_to_49 = fte_age_30_to_49/fte * 100,
    
    # COMPUTE LARGER AGE GROUPS: OVER 49
    # add up all groups that are combined in the larger group and divide by TOTAL hc or fte
    hc_age_over_49 = rowSums(select(., "hc_age_50_to_59", "hc_age_60_and_over"), na.rm = T),
    hc_perc_age_over_49 = hc_age_over_49/hc * 100,
    fte_age_over_49 = rowSums(select(., "fte_age_50_to_59", "fte_age_60_and_over"), na.rm = T),
    fte_perc_age_over_49 = fte_age_over_49/fte * 100,
    
    # estimate average age of teachers at a school
    # 1. use the hc or fte in each category, multiply it by the category midpoint
    # 2. add up across categories 
    # 3. divide by hc or fte for KNOWN categories
    # 4. this INCLUDES any hc_age_Age_unclassified or fte_age_Age_unclassified
    hc_avg_age      = (hc_age_Under_25 * 22.5 + hc_age_25_to_29 * 27 + hc_age_30_to_39 * 34.5 + 
                         hc_age_40_to_49 * 44.5 + hc_age_50_to_59 * 54.5 + hc_age_60_and_over * 62.5)/hc,
    fte_avg_age     = (fte_age_Under_25 * 22.5 + fte_age_25_to_29 * 27 + fte_age_30_to_39 * 34.5 + 
                         fte_age_40_to_49 * 44.5 + fte_age_50_to_59 * 54.5 + fte_age_60_and_over * 62.5)/fte,
    
    # estimate average age of teachers at a school
    # 1. use the hc or fte in each category, multiply it by the category midpoint
    # 2. add up across categories 
    # 3. divide by hc or fte for KNOWN categories
    # 4. this OMITS any hc_age_Age_unclassified or fte_age_Age_unclassified
    hc_avg_age_known  = (hc_age_Under_25 * 22.5 + hc_age_25_to_29 * 27 + hc_age_30_to_39 * 34.5 + 
                           hc_age_40_to_49 * 44.5 + hc_age_50_to_59 * 54.5 + hc_age_60_and_over * 62.5)
    /(hc_age_Under_25 + hc_age_25_to_29 + hc_age_30_to_39 + hc_age_40_to_49 + hc_age_50_to_59 + hc_age_60_and_over),
    fte_avg_age_known = (fte_age_Under_25 * 22.5 + fte_age_25_to_29 * 27 + fte_age_30_to_39 * 34.5 + 
                           fte_age_40_to_49 * 44.5 + fte_age_50_to_59 * 54.5 + fte_age_60_and_over * 62.5)
    /(fte_age_Under_25 + fte_age_25_to_29 + fte_age_30_to_39 + fte_age_40_to_49 + fte_age_50_to_59 + fte_age_60_and_over)
  ) %>% 
  # SELECT columns to keep
  select(matches("time|urn|laestab|school|Female|White|British|Classroom|age")) %>%
  select(matches("time_period|urn|laestab|school|fte_perc|avg_age")) %>%
  as.data.frame()

apply(wtc, 2, function(x) {sum(is.na(x))})


## ISSUE WITH AVERAGE AGE ##
wtc$tmp <- wtc$fte_avg_age - wtc$fte_avg_age_known # determine how much we underestimated the age
wtc$tmp_rd <- round(wtc$tmp, 1) # round
# plot data that has noticeable underestimation
library(ggplot2)
wtc[wtc$tmp_rd != 0,] %>% 
  ggplot(aes(x = tmp)) + 
  geom_histogram(stat = "bin", binwidth = 1, boundary = 1) + 
  stat_bin(geom = 'text', aes(label = after_stat(count)), binwidth = 1, boundary = 1, vjust = -.5)
wtc$tmp <- NULL
wtc$tmp_rd <- NULL

# COMBINE ALL DFs #


# process data
df <- swf %>%
  full_join(., wtc, by = id_cols) %>%
  # merge all dfs
  full_join(., ptr, by = id_cols) %>%
  full_join(., pay, by = id_cols) %>% 
  full_join(., vac, by = id_cols) %>%
  full_join(., abs, by = id_cols) %>%
  full_join(., tto, by = id_cols) %>%
  # replace with NAs
  mutate(
    # replace spaces and %
    across(where(is.character), ~gsub("^\\s+|\\s+$|%", "", .)),
    # x = not available - information has not been collected or there are no estimates available at this level of aggregation.
    across(where(is.character), ~na_if(., "x")), 
    # z = not applicable - statistic cannot be produced. For example where a denominator is not available to produce a percentage.
    across(where(is.character), ~na_if(., "z")), 
    # c = confidential - where presentation of data would disclose confidential information
    across(where(is.character), ~na_if(., "c")), 
    # u = low reliability - values of the potentially low quality, for example where values of statistical significance have been calculated.
    across(where(is.character), ~na_if(., "u")), 
    # replace comma and make numeric
    across(fte_all_teachers:last_col(), \(x) as.numeric(gsub(",", "", x)))) %>%
  # re-compute ratios
  mutate(pupil_to_qual_teacher_ratio = pupils_fte / qualified_teachers_fte ,
         pupil_to_qual_unqual_teacher_ratio = pupils_fte / teachers_fte,
         pupil_to_adult_ratio = pupils_fte / adults_fte) %>%
  # sort data
  arrange(laestab, time_period) %>% as.data.frame()

# CHECK FTE IN TEACHER TURNOVER #

check <- df[, grepl("urn|time_period|fte|left|remain", names(df))]
check <- check[, grepl("urn|time_period|teach|left|remain", names(check))]
check <- check[, !grepl("urn_|assistants|grade", names(check))]

# fte_all_teachers              FTE of all teachers (Filename: workforce_2010_2024_fte_hc_nat_reg_la_sch.csv)
# fte_classroom_teachers        FTE of classroom teachers (Filename: workforce_2010_2024_fte_hc_nat_reg_la_sch.csv)
# fte_leadership_teachers       FTE of leadership teachers (Filename: workforce_2010_2024_fte_hc_nat_reg_la_sch.csv)
# fte_head_teachers             FTE of head teachers (Filename: workforce_2010_2024_fte_hc_nat_reg_la_sch.csv)
# fte_deputy_head_teachers      FTE of deputy teachers (Filename: workforce_2010_2024_fte_hc_nat_reg_la_sch.csv)
# fte_assistant_head_teachers   FTE of assistant head teachers (Filename: workforce_2010_2024_fte_hc_nat_reg_la_sch.csv)
# fte_all_teachers_without_qts  FTE of all teachers without qualified teacher status (Filename: workforce_2010_2024_fte_hc_nat_reg_la_sch.csv)
# fte_leadership_non_teachers   FTE of leadership non-teachers (Filename: workforce_2010_2024_fte_hc_nat_reg_la_sch.csv)
# qualified_teachers_fte        Number of FTE qualified teachers (Filename: workforce_ptrs_2010_2024_nat_reg_la.csv)
# teachers_fte                  Number of FTE teachers (Filename: workforce_ptrs_2010_2024_nat_reg_la.csv)
# teacher_fte_in_census_year    FTE number of teachers in the census year (Filename: teacher_turnover_2010_2024_sch.csv)

# fte_all_teachers = fte_classroom_teachers + fte_leadership_teachers
check$fte_all_teachers_RECALC <- check$fte_classroom_teachers + check$fte_leadership_teachers
check$diff_fte_all_teachers <- check$fte_all_teachers - check$fte_all_teachers_RECALC

# fte_leadership_teachers = fte_head_teachers + fte_deputy_head_teachers + fte_assistant_head_teachers
check$fte_leadership_teachers_RECALC <- check$fte_head_teachers + check$fte_deputy_head_teachers + check$fte_assistant_head_teachers
check$diff_fte_leadership_teachers <- check$fte_leadership_teachers - check$fte_leadership_teachers_RECALC

# is fte_all_teachers equal to teachers_fte?
check$diff_fte_teach_ptr <- round(check$fte_all_teachers - check$teachers_fte, 2)
sum(check$diff_fte_teach_ptr != 0, na.rm = T) # fte_all_teachers is equal to teachers_fte in all but 7 instances


# teacher_fte_in_census_year = remained_in_the_same_school + left_the_state_funded_system + left_to_another_state_funded_school
check$teacher_fte_in_census_year_RECALC <- check$remained_in_the_same_school + check$left_the_state_funded_system + check$left_to_another_state_funded_school
check$diff_teacher_fte_in_census_year <- check$teacher_fte_in_census_year - check$teacher_fte_in_census_year_RECALC
sum(check$diff_teacher_fte_in_census_year != 0, na.rm = T) # fte_all_teachers is NOT equal to teacher_fte_in_census_year in 172871 instances
sum(check$diff_teacher_fte_in_census_year != 0, na.rm = T)/nrow(check) # fte_all_teachers is NOT equal to teacher_fte_in_census_year roughly half of the data
psych::describeBy(check$diff_teacher_fte_in_census_year, group = check$time_period)


# is fte_all_teachers equal to teacher_fte_in_census_year?
check$diff_fte_teach_tto <- round(check$fte_all_teachers - check$teacher_fte_in_census_year, 2)
sum(check$diff_fte_teach_tto != 0, na.rm = T) # fte_all_teachers is NOT equal to teacher_fte_in_census_year in 172871 instances
sum(check$diff_fte_teach_tto != 0, na.rm = T)/nrow(check) # fte_all_teachers is NOT equal to teacher_fte_in_census_year roughly half of the data
psych::describe(check$diff_fte_teach_tto)
psych::describeBy(check$diff_fte_teach_tto, group = check$time_period)

# save data
data.table::fwrite(df, file = file.path(dir_data, "data_swf.csv"), row.names = F)
