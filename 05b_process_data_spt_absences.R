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
na_values <- c("SUPP", "NP", "")

# Get Information about Schools #

# read in establishment data
gias <- as.data.frame(fread(file.path(dir_data, "data_gias_search.csv"), encoding = "UTF-8"))

# remove all establishments without an laestab (i.e., Children's centres, British schools overseas, Online providers)
gias <- gias[!is.na(gias$laestab), ]

# select relevant laestab and urn only and relevant columns
gias <- gias[!duplicated(gias), c("laestab", "urn", "establishmentname")]
names(gias) <- c("laestab", "urn_gias", "school")


# LOOP OVER ALL YEARS 
for (i in seq_along(start:finish)) {
  
  year = c(start:finish)[i]  
  
  # determine academic year
  academic_year <- paste0(year,"-", year+1)
  time_period <- as.numeric(gsub("-20", "", academic_year))
  
  # determine folder for academic year
  dir_year <- file.path(dir_in, academic_year)
  
  # skip covid years
  if(year == 2019 | year == 2020) next
  
  # read in absences data
  file = file.path(dir_year, paste0(academic_year, "_england_abs.csv"))
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
  
  # Convert to data.frame
  tmp <- as.data.frame(tmp)

  # change col names
  names(tmp) <- tolower(gsub("X...", "", names(tmp), fixed = T))
  names(tmp) <- gsub("x..", "perc.", names(tmp), fixed = T)
  names(tmp) <- gsub(".", "_", names(tmp), fixed = T)
  names(tmp) <- gsub(" ", "_", names(tmp), fixed = T)
  
  # Exclude rows that fully consist of NA values
  tmp <- tmp[apply(tmp, 1, function(row) !all(is.na(row))), ]
  
  # add year
  tmp$time_period <- time_period
  
  # check for any strings
  cat(academic_year, "\n")
  print(apply(tmp, 2, function(x) { unique(regmatches(x, gregexpr("[A-Za-z]+", x)))   }))
  
  # exclude national-level data
  tmp <- tmp %>% filter(! toupper(la) %in% c("NAT"))
  
  # combine across years
  if (year == start) {
    abs <- tmp
  } else {
    abs <- rbind.all.columns(abs, tmp)
  }
  
}

# create LAESTAB where possible
abs$laestab <- ifelse(!is.na(abs$la) & !is.na(abs$estab),
                          as.numeric(paste0(abs$la, abs$estab)),
                          NA)

# check urns and clean up data
abs <- cleanup_data(data_in = abs)

# select vars
abs <- abs[, c("time_period", "school", "laestab", "urn", "urn_abs", "perctot", "ppersabs10", "ppersabs15", "ppersabs20")]

# save data
abs <- abs[with(abs, order(laestab, time_period)),]
data.table::fwrite(abs, file = file.path(dir_data, "data_spt_absences.csv"), row.names = F)
