rm(list=ls())
gc()

###############################
##### WEB SCRAPING SCRIPT #####
###############################


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

# function to set up the environment
setup_environment <- function() {
  # Prevent scientific notation
  options(scipen = 999)
  
  # Empty the workspace in the global environment except for this function
  current_function <- c("setup_environment", "source_code")
  rm(list = setdiff(ls(envir = .GlobalEnv), current_function), envir = .GlobalEnv)
  
  # Define the directory
  dir <- getwd()
  
  # Create subfolders if they do not exist
  dir_data <- file.path(dir, "data")
  if (!dir.exists(dir_data)) {
    dir.create(dir_data)
  }
  
  dir_misc <- file.path(dir, "misc")
  if (!dir.exists(dir_misc)) {
    dir.create(dir_misc)
  }
  
  # Load necessary libraries
  library(rvest)
  library(xml2)
  library(httr)
  library(chromote)
  
  # Source external functions
  source_code(target_repo = "DfE_data", file_name = "functions.R")
  
  # Determine header information
  headers <- c(
    `user-agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/102.0.5005.61 Safari/537.36'
  )

  # Export headers and directories to the global environment
  assign("headers", headers, envir = .GlobalEnv)
  assign("dir_data", dir_data, envir = .GlobalEnv)
  assign("dir_misc", dir_misc, envir = .GlobalEnv)
}

##### School workforce in England ##### 

# This release is largely based on the School Workforce Census (SWC). 
# The census, which runs each November, collects information from schools and local 
# authorities on the school workforce in state-funded schools in England.
 
# Independent schools, non-maintained special schools, sixth-form colleges 
# and further education establishments are not included in the SWC.

# This release includes information on teaching and support staff, their characteristics, 
# teacher retention and pay, qualifications and details of the subjects taught in secondary schools.

# run all setups (reset environment)
setup_environment()

# download data
webscrape_government_data(dir_out =  file.path(dir_data, "school-workforce-in-england"),
                          parent_url = "https://explore-education-statistics.service.gov.uk/find-statistics/school-workforce-in-england",
                          pattern_to_match = glob2rx("*school-workforce*england/20*|*school-workforce*november-20*"))


##### Schools, pupils and their characteristics ##### 

# This release contains the latest statistics on school and pupil numbers and their characteristics, 
# including age, gender, free school meals (FSM) eligibility, English as an additional language, 
# ethnicity, school characteristics and class sizes.

# The publication combines information from the January (spring) school census, 
# school level annual school census, general hospital school census and alternative provision census.

# run all setups (reset environment)
setup_environment()

# download data
webscrape_government_data(dir_out =  file.path(dir_data, "school-pupils-and-their-characteristics"),
                          parent_url = "https://explore-education-statistics.service.gov.uk/find-statistics/school-pupils-and-their-characteristics",
                          pattern_to_match = glob2rx("*school-pupils-and-their-characteristics/20*|*schools-pupils*20*"))


##### Special educational needs in England ##### 

# run all setups (reset environment)
setup_environment()

# download data
webscrape_government_data(dir_out =  file.path(dir_data, "special-educational-needs-in-england"),
                          parent_url = "https://explore-education-statistics.service.gov.uk/find-statistics/special-educational-needs-in-england",
                          pattern_to_match = glob2rx("*special-educational-needs-in-england/20*|*special-educational-needs-in-england-january-20**"))


##### Performance tables #####

# Call the function to run all setups and store returned values
setup_environment()

# determine output directory
dir_out <- file.path(dir_data, "performance-tables")
if (!dir.exists(dir_out)) {
  dir.create(dir_out)
}

# determine years of interest
start <- 2010
finish <- 2023
# start <- 2018
# finish <- 2018

# Create an empty vector to store failed URLs
failed_urls <- character()


for (year in start:finish) {
  
  # skip covid year
  if(year == 2019) next
  
  # determine academic year
  academic_year <- paste0(year,"-", year+1)
  cat("\n\n#############", academic_year, "#############\n\n")
  
  # create folder for academic year
  dir_year <- file.path(dir_out, academic_year)
  
  if (!dir.exists(dir_year)) {
    dir.create(dir_year)
  }
  
  # determine datasets of interest based on year
  if (year < 2018) {
    datasets = c("KS2", "KS4", "KS5", "PUPILABSENCE", "CENSUS", "SPINE")
  } else if (year == 2020) {
    datasets = c("CENSUS", "GIAS")
  } else if (year == 2021) {
    datasets = c("KS4", "KS5", "PUPILABSENCE", "CENSUS", "GIAS")
  } else {
    datasets = c("KS2", "KS4", "KS5", "PUPILABSENCE", "CENSUS", "GIAS")
  }
  

  for (d in 1:length(datasets)) {
    
    # get data files #
    
    # specify URL
    url_data <- paste0("https://www.compare-school-performance.service.gov.uk/download-data?download=true&regions=0&filters=", datasets[d], "&fileformat=csv&year=", academic_year,"&meta=false")
    
    # Try downloading data URL
    tryCatch({
      download_data_from_url(url = url_data)
    }, error = function(e) {
      cat("Failed to download URL:", url_data, "\nError message:", e$message, "\n")
      failed_urls <<- c(failed_urls, url_data)
    })
    
    # get meta data #
    
    # specify URL
    url_meta <- paste0("https://www.compare-school-performance.service.gov.uk/download-data?download=true&regions=", datasets[d], "&filters=meta&fileformat=csv&year=", academic_year,"&meta=true")
    if (year == 2014 & datasets[d] == "PUPILABSENCE") {
      url_meta <- paste0("https://www.compare-school-performance.service.gov.uk/download-data?download=true&regions=GUIDANCE&filters=meta&fileformat=zip&year=", academic_year,"&meta=true")
    }
    
    # Try downloading meta URL
    tryCatch({
      download_data_from_url(url = url_meta)
    }, error = function(e) {
      cat("Failed to download URL:", url_meta, "\nError message:", e$message, "\n")
      failed_urls <<- c(failed_urls, url_meta)
    })
    
    if (year == 2014 & datasets[d] == "PUPILABSENCE") {
      # move guidance data in separate folder, currently saved in dir_out
      tmp_dir <- file.path(dir_out, academic_year, "guidance_meta")
      dir.create(tmp_dir)
      # get all files
      tmp_files <- list.files(dir_out, pattern = ".pdf|.xls", full.names = T)
      # determine new names
      tmp_files_new <- gsub("performance-tables", paste0("performance-tables/", academic_year, "/guidance_meta"), tmp_files)
      # move
      file.rename(tmp_files, tmp_files_new)
    }
    
  }
  
  # get guidance data #
  if (year %in% c(2015, 2016, 2017, 2018)) {
    
    # specify URL
    url_guid <- paste0("https://www.compare-school-performance.service.gov.uk/download-data?download=true&regions=GUIDANCE&filters=meta&fileformat=zip&year=", academic_year,"&meta=true")
    
    # Try downloading data URL
    tryCatch({
      download_data_from_url(url = url_guid)
    }, error = function(e) {
      cat("Failed to download URL:", url_guid, "\nError message:", e$message, "\n")
      failed_urls <<- c(failed_urls, url_guid)
    })
    
    if(year == 2017){
      
      source_folder <- file.path(dir_out, paste0(academic_year, "_guidance_meta")) 
      
      # Copy the folder and its contents to the new location
      if (file.copy(source_folder, file.path(dir_out, academic_year), recursive = TRUE)) {
        cat("Folder successfully copied.\n")
        
        # Delete the original folder
        if (unlink(source_folder, recursive = TRUE)) {
          cat("Original folder successfully deleted.\n")
        } else {
          cat("Failed to delete the original folder.\n")
        }
      }
    } else if(year == 2018){
      
      source_folder <- file.path(dir_out, "2019 KS4 Guidance")
      # Copy the folder and its contents to the new location
      if (file.copy(source_folder, file.path(dir_out, academic_year), recursive = TRUE)) {
        cat("Folder successfully copied.\n")
        
        # Delete the original folder
        if (unlink(source_folder, recursive = TRUE)) {
          cat("Original folder successfully deleted.\n")
        } else {
          cat("Failed to delete the original folder.\n")
        }
      }
      source_folder <- file.path(dir_out, "2019 16-18 Guidance")
      # Copy the folder and its contents to the new location
      if (file.copy(source_folder, file.path(dir_out, academic_year), recursive = TRUE)) {
        cat("Folder successfully copied.\n")
        
        # Delete the original folder
        if (unlink(source_folder, recursive = TRUE)) {
          cat("Original folder successfully deleted.\n")
        } else {
          cat("Failed to delete the original folder.\n")
        }
      }
    } else {
      # move guidance data in separate folder, currently saved in dir_out
      tmp_dir <- file.path(dir_out, academic_year, "guidance_meta")
      dir.create(tmp_dir)
      # get all files
      tmp_files <- list.files(dir_out, pattern = ".pdf|.xls|.ods|.doc", full.names = T)
      # determine new names
      tmp_files_new <- gsub("performance-tables", paste0("performance-tables/", academic_year, "/guidance_meta"), tmp_files)
      # move
      file.rename(tmp_files, tmp_files_new)
    }

  }
  
  # remove variable from environment
  rm(dir_year)
}

# After the loop, check if any URLs failed to download
if (length(failed_urls) > 0) {
  cat("\nThe following URLs failed to download:\n")
  print(failed_urls)
} else {
  cat("\nAll URLs downloaded successfully.\n")
}

