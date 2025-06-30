#### ALL-PURPOSE HELPER FUNCTIONS ####

# source code
source_code(target_repo = "helper_functions", file_name = "functions.R")

#### PROJECT-SPECIFIC FUNCTIONS ####

merge_timelines_across_columns <- function(data_in = df_in,
                                           column_vector = "cols_to_merge",
                                           stem = "new_var", 
                                           identifier_columns = "id_cols",
                                           data_out = df_out) {
  
  data_out <- data_in %>% 
    # select columns
    select(all_of(c(identifier_columns, column_vector))) %>%
    # replace any NAs with ""
    mutate(across(all_of(column_vector), ~ifelse(is.na(.), "", .))) %>%
    # merge information across cols using paste
    tidyr::unite("tmp", all_of(column_vector), na.rm = TRUE, remove = FALSE, sep = "") %>%
    # create column that contains tag with information about the column data retained
    mutate(across(all_of(column_vector), ~ifelse(. != "", deparse(substitute(.)), ""))) %>%
    tidyr::unite("tag", all_of(column_vector), na.rm = TRUE, remove = TRUE, sep = "") %>%
    mutate(
      # replace "" with NA
      across(c(tmp, tag), ~na_if(., "")),
      # make new variable numeric
      tmp = as.numeric(tmp)) %>%
    # change col names
    rename_with(~c(stem, paste0(stem, "_tag")), c(tmp, tag)) %>%
    # merge with data_out
    full_join(x = data_out, y = ., by = identifier_columns) %>%
    as.data.frame()
  
  return(data_out)
  
}


merge_staggered_timelines_across_columns <- function(data_in = df_in,
                                                     column_vector = "cols_to_merge",
                                                     stem = "new_var", 
                                                     variable_levels = "new_levels",
                                                     identifier_columns = "id_cols",
                                                     data_out = df_out) {
  
  # select columns
  tmp <- data_in[, c(identifier_columns, column_vector)]
  
  # determine mapping
  mapping <- data.frame(old = column_vector,
                        new = variable_levels)
  cat("Applied mapping from column_vector to variable_levels:\n\n")
  print(mapping)
  
  tag = paste0(stem, "_tag")
  
  # use dplyr
  tmp <- tmp %>%
    # apply grouping by identifier variable
    group_by(.data[[identifier_columns]]) %>%
    # replace every NA with the unique value observed for each group
    mutate_at(column_vector, function(x) {ifelse(is.na(x), unique(x[!is.na(x)]), x)}) %>%
    # remove all duplicated columns
    distinct(., .keep_all = TRUE) %>%
    
    # transform into long format
    reshape2::melt(id = identifier_columns, variable.name = tag, value.name = stem) %>%
    # change variable levels
    mutate(time_period = plyr::mapvalues(get(tag), column_vector, variable_levels, warn_missing = TRUE)) %>%
    # make numeric
    mutate_at(c(identifier_columns, "time_period"), ~as.numeric(as.character(.)))
  
  
  # merge with data_out
  data_out <- merge(data_out, tmp, by = id_cols, all = T)
  rm(tmp)
  
  return(data_out)
}


### web scraping ###

# Function to identify year of release
get_year <- function(input_url){
  
  # Split the URLs into parts
  parts <- unlist(strsplit(input_url, "[[:punct:]]"))
  
  # Find the year indices
  idx <- grep("(^20[0-2][0-9]$|^2[0-9]$)", parts)
  
  # check if it contains a year
  if (identical(idx, integer(0)) == F) {
    # if so, return year
    year <- paste(parts[idx], collapse = "-")
  } else {
    year <- character(0)
  }
  
  return(year)
}

assign_dir_year <- function(x, input_url = "url") assign(x, file.path(dir_out, get_year(input_url)),envir=globalenv())

# functions
is.sequential <- function(x){
  all(abs(diff(x)) == 1)
} 

# Function to handle overlapping parts and convert relative URLs to absolute URLs
resolve_url <- function(base_url, relative_url) {
  if (!grepl("^http", relative_url)) {
    base_url <- sub("/$", "", base_url)
    relative_url <- sub("^/", "", relative_url)
    
    base_parts <- unlist(strsplit(base_url, "/"))
    relative_parts <- unlist(strsplit(relative_url, "/"))
    
    overlap_index <- which(base_parts %in% relative_parts)
    
    # Handle empty overlap_index
    if (length(overlap_index) == 0) {
      return(paste0(base_url, "/", relative_url))
    }
    
    pre_overlap_base <- min(overlap_index) - 1
    if (pre_overlap_base < 1) {
      base_unique <- character(0)
    } else {
      base_unique <- base_parts[1:pre_overlap_base]
    }
    
    base_overlap <- base_parts[overlap_index]
    relative_unique <- relative_parts[!relative_parts %in% base_parts]
    
    # Construct URL safely
    segments <- c(
      paste0(base_unique, collapse = "/"),
      paste0(base_overlap, collapse = "/"),
      paste0(relative_unique, collapse = "/")
    )
    absolute_url <- paste(segments[segments != ""], collapse = "/")
    
    return(absolute_url)
  } else {
    return(relative_url)
  }
}

# Function to handle url redirects
get_final_url <- function(url) {
  response <- GET(url, timeout(10))  # Timeout prevents hanging
  if (status_code(response) == 200) {
    return(response$url)
  } else {
    warning("Failed to resolve URL: HTTP ", status_code(response))
    return(url)  # Fallback to original URL
  }
}

# function to download data from an URL that directly links to a file
download_data_from_url <- function(url){
  
  # determine header information
  headers = c(
    `user-agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/102.0.5005.61 Safari/537.36'
  )
  
  
  max_attempts <- 5
  successful <- FALSE
  
  for (attempt in 1:max_attempts) {
    request <- try(httr::GET(url = url, httr::add_headers(.headers = headers)), silent = TRUE)
    
    if (inherits(request, "try-error")) {
      cat("\nHTTP request failed on attempt", attempt, "with an error. Retrying in", 2^(attempt-1), "seconds...\n")
      Sys.sleep(2^(attempt-1))
    } else {
      status_code <- httr::status_code(request)
      if (status_code != 200) {
        # Check if the error is likely URL-related (client error)
        if (status_code == 404) {
          cat("\nHTTP request failed on attempt", attempt, "with status code", status_code, 
              "indicating a likely issue with the URL. Aborting.\n")
          stop("URL Error: ", status_code)
        } else {
          cat("\nHTTP request returned status", status_code, "on attempt", attempt, 
              ". Retrying in", 2^(attempt-1), "seconds...\n")
          Sys.sleep(2^(attempt-1))
        }
      } else {
        successful <- TRUE
        break
      }
    }
  }
  
  if (!successful) {
    stop("Failed to retrieve data after ", max_attempts, " attempts.")
  }
  
  # retrieve information from URL 
  # request <- httr::GET(url = url, httr::add_headers(.headers=headers))
  # request <- httr::GET(url = url_meta, httr::add_headers(.headers=headers))
  # request <- httr::GET(url = url_data, httr::add_headers(.headers=headers))
  
  # retrieve header information
  input <- request$headers$`content-disposition`
  
  # check for file name information
  if (!is.null(input) && nzchar(input)) { # input exists and is non-empty
    
    if (grepl("'", input, perl = TRUE)) {
      tmp <- sub(".*'", "", input)
      tmp <- sub("%2F", "_", tmp)
    } else {
      tmp <- sub('[^\"]+\"([^\"]+).*', '\\1', input)
    }
    tmp <- ifelse(nchar(tmp) > 100, gsub("_20", "", tmp), tmp) # replace if filename is too long
    
  } else {
    
    # Set a default filename or handle the error appropriately
    cat("\nWarning: 'content-disposition' header is missing. Using a default filename.\n")
    tmp <- paste0("download_", url, ".bin")
    
  }
  
  # check if higher level variable dir_year exists in environment
  if (!exists("dir_year")){
    
    # get year from url
    assign_dir_year("dir_year_data", url)
    
    # if url does not contain a year
    if (identical(dir_year_data, character(0)) == T) {
      
      # check headers$`content-disposition`
      assign_dir_year("dir_year_data", tmp)
    }
    
  }
  
  # information on dir_year would come from a higher level in the script
  if (!exists("dir_year") & exists("dir_year_data")){ # dir_year does not exist, but dir_year_data was created
    
    if (identical(dir_year_data, character(0)) == T) { # check it's not empty
      
      # if empty, ABORT & print to console
      cat("\nNo information on year available, character is empty\n")
      cat("\nSkipped file", tmp, "\n")
      return(NULL)
      
    } else {
      
      # if not empty, create directory to save data
      if (!dir.exists(dir_year_data)) {
        dir.create(dir_year_data)
      }
      # declare dir_year_data to be the higher level dir_year variable
      dir_year <- dir_year_data
      
    }
    
  }
  
  # determine file name
  # if (grepl('[^/]', tmp, perl = T)) {
  #   dir_ex <- sub("/[^/]+$", "", dir_year)
  # } else {
  #   dir_ex <- dir_year
  # }
  # 
  # file_name <- file.path(dir_ex, tmp)
  file_name <- file.path(dir_year, tmp)
  
  # retrieve raw content from request
  cat("\nDownloading file from url...\n")
  cat("\t", url, "\n")
  cat("\tto", file_name, "\n")
  bin <- content(request, "raw")
  
  # write binary data to file
  writeBin(bin, file_name)
  while (!file.exists(file_name)) {
    Sys.sleep(1)
  }
  
  cat("\t...done\n")
  
  
  # unzip folder
  if (grepl("zip", file_name)) {
    
    cat("Unzipping...\n")
    cat("\t...done\n")
    
    if (grepl("performance-tables", dir_year)) {
      # Remove everything after the last / from directory
      dir_ex <- sub("/[^/]+$", "", dir_year)
    } else {
      dir_ex <- dir_year
    }
    
    unzipped <- unzip(file_name, exdir = dir_ex)
    file.remove(file_name)
  }
  
}

# function to scrape a website for file download links that also downloads all linked files
webscrape_government_data <- function(dir_out = "path_to_directory",
                                      parent_url = "url",
                                      pattern_to_match = "pattern"){
  
  # create output dir
  if (!dir.exists(dir_out)) {
    dir.create(dir_out)
  }
  
  assign("dir_out", dir_out, envir=globalenv())
  
  # Read the webpage content
  webpage <- read_html(parent_url)
  
  # Extract all the links from the webpage
  links <- webpage %>%
    html_nodes("a") %>%  # Select all <a> tags
    html_attr("href")    # Extract the href attribute
  
  # Apply function to resolve relative URLs to all links
  absolute_links <- sapply(links, function(link) {
    resolve_url(parent_url, link)
  })
  
  # check if there are any application/octet-stream absolute_links
  download_links <-  unique(absolute_links[grepl("/files$", absolute_links)])
  # download_links <-  unique(absolute_links[grepl("/files$|content.explore|data-catalogue", absolute_links)])
  # download_links <-  unique(absolute_links[grepl("/files", absolute_links)])

  if (identical(download_links, character(0)) == F) {
    cat("\nFound download links on parent URL...\n")
    cat("\t", download_links, sep = "\n\t")
    cat("\n")
    # if so, download
    sapply(download_links, download_data_from_url)
  }
  
  # Filter the links using the specified pattern
  release_links <- unique(links[grepl(pattern_to_match, links)])
  
  if (grepl("school-pupils-and-their-characteristics", parent_url)) {
    # data not linked there
    release_links <- sort(c(release_links, "https://www.gov.uk/government/statistics/schools-pupils-and-their-characteristics-january-2018", "https://www.gov.uk/government/statistics/schools-pupils-and-their-characteristics-january-2019"))
  }
  
  
  # check if there are any matching links
  if (identical(release_links, character(0)) == T) {
    cat("NO MATCHES FOUND")
    cat(release_links)
    cat(pattern_to_match)
    
  } else {
    
    # Apply the function to deal with relative urls to all release links
    release_links <- sapply(release_links, function(link) {
      resolve_url(parent_url, link)
    })
    
    # Output the release links to the console
    cat("\nLooping over these release links\n")
    cat("\t", release_links, sep = "\n\t")
    cat("\n")
    
    # loop over all releases
    for (release_url in release_links) {
      
      #release_url <- release_links[1]
      
      # create folder for year of release
      assign_dir_year("dir_year", file.path(dir_out, get_year(release_url)))
      
      if (!dir.exists(dir_year)) {
        dir.create(dir_year)
      }
      
      cat("\nReading content of release landing page", release_url, "\n")
      
      # Read the webpage content
      webpage <- read_html(release_url)
      
      # Extract all the links from the webpage
      links <- webpage %>%
        html_nodes("a") %>%  # Select all <a> tags
        html_attr("href")    # Extract the href attribute
      
      # Filter the download links (e.g., links ending with .pdf)
      download_links <- links[grepl("\\.[a-zA-Z]+$|/files$", links)]
      download_links <- download_links[!grepl(".uk$", download_links)]
      
      # Remove duplicates
      download_links <- unique(download_links)
      
      if (identical(download_links, character(0)) == F) {
        cat("\nFound download links on release URL...\n")
        cat("\t", download_links, sep = "\n\t")
        cat("\n")
        # if so, download
        sapply(download_links, download_data_from_url)
      }
      
    }
    
  }
  
}

# function to fix roundings
# rounding applied to nearest 5 in some publications, but not in others
# this causes inconsistencies across different datasets
fix_roundings <- function(var_nrd = "variable_not_rounded", var_rd = "variable_rounded",
                          new_var = "",
                          identifier_columns = "id_cols",
                          col_to_filter = "col_name",
                          filter = vector,
                          rounding_factor = 5,
                          data_in = df_in) {
  # select columns
  tmp <- data_in[, c(identifier_columns, var_nrd, var_rd)]
  
  # compute difference in raw values
  tmp$diff <- tmp[, var_nrd] - tmp[, var_rd]
  
  # round variable currently not rounded
  tmp$rd <- round(tmp[, var_nrd] / rounding_factor) * rounding_factor
  
  # replace any instances of rounded values with unrounded values
  tmp$test <- ifelse(tmp[, var_nrd] != 0 & tmp[, col_to_filter] %in% filter, tmp[, var_nrd], tmp[, var_rd])
  
  # compute diff after replacing rounded values with unrounded values
  tmp$diff2 <- tmp[, var_nrd] - tmp$test
  
  # fix rounding issues
  if (new_var != "") {
    tmp[, new_var] <- tmp$test
  } else {
    tmp[, paste0(var_rd, "_orig")] <- tmp[, var_rd] # copy original unrounded values
    tmp[, var_rd] <- tmp$test
  }
  
  return(tmp)
}

# Function to determine the URN of an establishment in a given academic year
get_urn <- function(data, laestab, academic_year_start) {
  # Define the start and end dates of the academic year
  academic_start <- as.Date(paste0(academic_year_start, "-09-01"))
  academic_end <- as.Date(paste0(academic_year_start + 1, "-08-31"))
  
  # Filter the data for the given establishment
  est_data <- data[data$laestab == laestab, ]
  
  # Check each row for the URN during the academic year
  for (i in 1:nrow(est_data)) {
    row <- est_data[i, ]
    open_date <- as.Date(row$opendate, format = "%Y-%m-%d")
    close_date <- as.Date(row$closedate, format = "%Y-%m-%d")
    
    if ((is.na(open_date) || open_date <= academic_end) && (is.na(close_date) || close_date >= academic_start)) {
      return(row$urn)
    }
  }
  
  return(NA)
}

# Create a new data frame to store the URN of each establishment for each academic year
create_urn_df <- function(data, start_year, end_year) {
  # Get a unique list of establishments
  establishments <- unique(data$laestab)
  
  # Create an empty data frame to store the results
  status_df <- data.frame(laestab = integer(), urn = integer(), academic_year = integer(), stringsAsFactors = FALSE)
  
  # Loop through each academic year and each establishment
  for (year in start_year:end_year) {
    for (est in establishments) {
      school_urn <- get_urn(data, est, year)
      status_df <- rbind(status_df, data.frame(time_period = year, laestab = est, urn = school_urn, stringsAsFactors = FALSE))
    }
  }
  
  return(status_df)
}

# urn / laestab lookup function
lookup_urn_laestab <- function() {
  
}