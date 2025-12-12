#### ALL-PURPOSE HELPER FUNCTIONS ####

# source code
source_code(target_repo = "helper_functions", file_name = "functions.R")

#### PROJECT-SPECIFIC FUNCTIONS ####

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

# Function to identify term of release
get_term <- function(input_url) {
  parts <- unlist(strsplit(input_url, "[[:punct:]]"))
  idx <- grep("autumn|spring|summer", parts, ignore.case = TRUE)
  if (length(idx) > 0) {
    term <- paste(parts[idx], collapse = "-")
    term <- paste0(term, "-term")
  } else {
    term <- character(0)
  }
  return(term)
}

assign_dir_term <- function(x, input_url = "url") assign(x, file.path(dir_out, get_year(input_url), get_term(input_url)),envir=globalenv())

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
download_data_from_url <- function(url) {
  headers = c(
    `user-agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/102.0.5005.61 Safari/537.36'
  )
  
  max_attempts <- 5
  successful <- FALSE
  
  for (attempt in 1:max_attempts) {
    message(sprintf("Attempt %d to download: %s", attempt, url))
    request <- try(httr::GET(url = url, httr::add_headers(.headers = headers)), silent = TRUE)
    
    if (inherits(request, "try-error")) {
      message(sprintf("HTTP request failed on attempt %d with an error. Retrying in %d seconds...", attempt, 2^(attempt-1)))
      Sys.sleep(2^(attempt-1))
    } else {
      status_code <- httr::status_code(request)
      if (status_code != 200) {
        if (status_code == 404) {
          message(sprintf("HTTP 404 error on attempt %d: URL likely invalid. Aborting.", attempt))
          stop("URL Error: 404 Not Found")
        } else {
          message(sprintf("HTTP status %d on attempt %d. Retrying in %d seconds...", status_code, attempt, 2^(attempt-1)))
          Sys.sleep(2^(attempt-1))
        }
      } else {
        successful <- TRUE
        break
      }
    }
  }
  
  if (!successful) {
    stop(sprintf("Failed to retrieve data after %d attempts.", max_attempts))
  }
  
  # Determine filename from headers or URL
  input <- request$headers$`content-disposition`
  if (!is.null(input) && nzchar(input)) {
    if (grepl("'", input, perl = TRUE)) {
      tmp <- sub(".*'", "", input)
      tmp <- sub("%2F", "_", tmp)
    } else {
      tmp <- sub('[^\"]+\"([^\"]+).*', '\\1', input)
    }
    if (nchar(tmp) > 100) {
      tmp <- gsub("_20", "", tmp)
    }
  } else {
    message("Warning: 'content-disposition' header missing. Using default filename.")
    tmp <- paste0("download_", basename(url))
  }
  
  # Create directory if needed
  if (!exists("dir_year")) {
    assign_dir_year("dir_year_data", url)
    if (length(dir_year_data) > 0 && nzchar(dir_year_data)) {
      message("No year information available. Skipping file ", tmp)
      return(NULL)
    } else {
      if (!dir.exists(dir_year_data)) dir.create(dir_year_data, recursive = TRUE)
      dir_year <- dir_year_data
    }
  }
  
  # determine dir_release
  dir_release <- 
    # if character(0) then dir_release = dir_year
    ifelse(identical(dir_term, character(0)), dir_year, 
           # if dir_term != dir_year then dir_release = dir_term else dir_release = dir_year
           ifelse(normalizePath(dir_term) != normalizePath(dir_year), dir_term, dir_year))
  

  file_name <- file.path(dir_release, tmp)
  
  message("Downloading file from URL...")
  message("\tURL: ", url)
  message("\tSaving to: ", file_name)
  
  bin <- httr::content(request, "raw")
  writeBin(bin, file_name)
  
  while (!file.exists(file_name)) {
    Sys.sleep(1)
  }
  
  message("Download complete: ", basename(file_name))
  
  # Unzip if ZIP file
  if (grepl("\\.zip$", file_name, ignore.case = TRUE)) {
    message("Unzipping file: ", basename(file_name))
    tryCatch({
      zip_list <- zip::zip_list(file_name)
      if (nrow(zip_list) == 0) stop("Empty ZIP file")
      zip::unzip(file_name, exdir = dir_release)
      rm(zip_list)
      gc()
      
      max_retries <- 3
      for (i in 1:max_retries) {
        if (file.exists(file_name)) {
          removal_status <- file.remove(file_name)
          if (removal_status) {
            message("ZIP file removed after extraction.")
            break
          }
          Sys.sleep(3)
        } else {
          break
        }
      }
      if (file.exists(file_name)) {
        warning("Failed to remove ZIP file after ", max_retries, " attempts. Please delete manually: ", file_name)
      }
      message("Unzipped files are saved in: ", unzip_dir)
    }, error = function(e) {
      warning("Unzip failed (", e$message, "). Keeping ZIP file for manual inspection.")
    })
  }
  
  return(invisible(file_name))
}

# function to automate downloading data via a button that triggers JavaScript (where the download link isn't in the HTML) using chromote
download_data_via_button_chromote <- function(
    url,
    download_dir = getwd(),
    button_selectors = c(".govuk-button", ".ChevronCard_link__I3925"),
    timeout = 600
) {
  
  b <- ChromoteSession$new()
  
  # Configure downloads
  b$Browser$setDownloadBehavior(
    behavior = "allow",
    downloadPath = normalizePath(download_dir)
  )
  
  # Navigate to page
  b$go_to(url)
  
  # Attempt button clicks
  button_clicked <- FALSE
  for (selector in button_selectors) {
    tryCatch({
      b$Runtime$evaluate(paste0('document.querySelector("', selector, '")?.click()'))
      message("Clicked button: ", selector)
      button_clicked <- TRUE
      break
    }, error = function(e) NULL)
  }
  
  if (!button_clicked) stop("No valid buttons found")
  
  # Monitor downloads
  start_time <- Sys.time()
  initial_files <- list.files(download_dir, full.names = TRUE)
  final_file <- NULL
  
  while (difftime(Sys.time(), start_time, units = "secs") < timeout) {
    current_files <- list.files(download_dir, full.names = TRUE)
    new_files <- setdiff(current_files, initial_files)
    completed_files <- new_files[!grepl("\\.crdownload$", new_files)]
    
    if (length(completed_files) > 0) {
      final_file <- completed_files[1]
      message("Downloaded: ", basename(final_file))
      break
    }
    Sys.sleep(1)
  }
  
  if (is.null(final_file)) stop("Download did not complete within timeout")
  
  # Unzip with integrity checks
  if (grepl("\\.zip$", final_file, ignore.case = TRUE)) {
    message("Unzipping: ", basename(final_file))
    tryCatch({
      
      # List Files in a 'zip' Archive
      zip_list <- zip::zip_list(final_file)
      
      if (nrow(zip_list) == 0) stop("Empty ZIP file")
      
      # Uncompress 'zip' Archives
      zip::unzip(final_file, exdir = download_dir)
      
      # Release file handle before deletion
      rm(zip_list)
      gc()
      
      # Robust removal with retries
      max_retries <- 3
      for (i in 1:max_retries) {
        if (file.exists(final_file)) {
          removal_status <- file.remove(final_file)
          if (removal_status) break
          Sys.sleep(3)
        } else break
      }
      if (file.exists(final_file)) {
        warning("Failed to remove ZIP file after ", max_retries, " attempts. ",
                "Please delete manually: ", final_file)
      }
      
      # List extracted files
      unzipped_files <- list.files(download_dir, recursive = TRUE, 
                                   pattern = "\\.(csv|xlsx|xls|txt|dat)$", 
                                   full.names = TRUE)
      message("Unzipped files: ", paste(basename(unzipped_files), collapse = ", "))
      return(invisible(unzipped_files))
    }, error = function(e) {
      warning("Unzip failed (", e$message, "). Keeping ZIP file for manual inspection.")
      return(final_file)
    })
  } else {
    return(final_file)
  }
  
  b$close()
  
}


# function to scrape a website for file download links that also downloads all linked files
webscrape_government_data <- function(dir_out = "path_to_directory",
                                      parent_url = "url",
                                      pattern_to_match = "pattern"){
  
  # Resolve final URL before scraping
  parent_url_final <- get_final_url(parent_url)
  
  # create output dir
  if (!dir.exists(dir_out)) {
    dir.create(dir_out)
  }
  
  assign("dir_out", dir_out, envir=globalenv())
  
  # Read the webpage content
  webpage <- read_html(parent_url_final)
  
  # Extract all the links from the webpage
  links <- webpage %>%
    html_nodes("a") %>%  # Select all <a> tags
    html_attr("href")    # Extract the href attribute

  # add redirected url to list of links
  if (parent_url != parent_url_final) {
    links <- c(parent_url_final, links)
  }
  
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
    invisible(sapply(download_links, download_data_from_url))
  }
  
  # Filter the links using the specified pattern
  release_links <- unique(absolute_links[grepl(pattern_to_match, absolute_links)])
  
  # remove some urls from list
  release_links <- release_links[!grepl("data-guidance|prerelease-access-list", release_links)]
  
  # check if there are any matching links
  if (identical(release_links, character(0)) == T) {
    cat("NO MATCHES FOUND")
    cat(release_links)
    cat(pattern_to_match)
    
  } else {
    
    # Output the release links to the console
    cat("\nLooping over these release links\n")
    cat("\t", release_links, sep = "\n\t")
    cat("\n")
    
    # loop over all releases
    for (release_url in release_links) {
      
      # release_url <- release_links[grepl("2017", release_links)]
      # release_url <- release_links[1]
      
      # get year 
      assign_dir_year("dir_year", release_url)
      # create output dir
      if (!dir.exists(dir_year)) dir.create(dir_year, recursive = TRUE)
      
      # get term
      assign_dir_term("dir_term", release_url)
      # if there is a term
      if(length(dir_term) > 0 && nzchar(dir_term)){ 
        # create output dir
        if (!dir.exists(dir_term)) dir.create(dir_term, recursive = TRUE)
        # define directory to download to
        dir_release <- if (normalizePath(dir_term) != normalizePath(dir_year)) dir_term else dir_year
      } else {
        dir_release <- dir_year
      }
      
      cat("\nReading content of release landing page", release_url, "\n")
      webpage <- read_html(release_url)
      
      # --- NEW: Check for "Download all data (ZIP)" button ---
      buttons <- html_nodes(webpage, "button")
      button_texts <- html_text(buttons, trim = TRUE)
      # Look for the button text (case-insensitive, ignore whitespace)
      has_download_zip <- any(grepl("^Download all data \\(ZIP?\\)$", button_texts, ignore.case = TRUE))
      
      if (has_download_zip) {
        cat("\nFound 'Download all data (ZIP)' button. Attempting automated download...\n")
        download_data_via_button_chromote(release_url, download_dir = dir_release)
      } else {
        
        # --- Fallback: Download from links as before ---
        
        # Extract all the links from the webpage
        links <- webpage %>%
          html_nodes("a") %>%  # Select all <a> tags
          html_attr("href")    # Extract the href attribute
        
        # Apply function to resolve relative URLs to all links
        absolute_links <- sapply(links, function(link) {
          resolve_url(parent_url, link)
        })
        
        # Filter the download links (e.g., links ending with .pdf)
        download_links <- absolute_links[grepl("\\.[a-zA-Z]+$|/files/", absolute_links)]
        download_links <- download_links[!grepl(".uk$", download_links)]
        download_links <- unique(download_links)
        
        # remove any csv-preview links
        download_links <- download_links[!grepl("csv-preview", download_links)]
        
        # attempt one more way to get download links
        if (length(download_links) == 0){
          download_links <- absolute_links[grepl("/files?fromPage=ReleaseDownloads", absolute_links, fixed = T)]
        }

        if (length(download_links) > 0) {
          cat("\nFound download links on release URL...\n")
          cat("\t", download_links, sep = "\n\t")
          cat("\n")
          invisible(sapply(download_links, download_data_from_url))
        } else {
          cat("\nNo download buttons or links found for this release.\n")
        }
      }
    }
    
  }
  
}

### data processing ###

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
  tmp$test <- ifelse(!is.na(tmp[, var_nrd]) & tmp[, var_nrd] != 0 & tmp[, col_to_filter] %in% filter, tmp[, var_nrd], tmp[, var_rd])
  
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
create_urn_laestab_lookup <- function(data_in = df, original_name = NULL) {
  
  # Use provided name or try to get it from substitute
  if (is.null(original_name)) {
    dataset_name <- deparse(substitute(data_in))
  } else {
    dataset_name <- original_name
  }
  
  # rename columns for consistency
  # this assumes that either laestab OR school_laestab are used as column names, NOT both
  if ("urn" %in% names(data_in)) {
    data_in <- data_in %>%
      rename(school_urn = urn)  
  }
  if ("laestab" %in% names(data_in)) {
    data_in <- data_in %>%
      rename(school_laestab = laestab)  
  }
  
  # Export the modified data to global environment
  assign(dataset_name, data_in, envir = .GlobalEnv)
  
  # extract all id pairings #
  # for each unique school_urn, check if it occurs in the gias
  # if it does not, then the URN is wrong
  ids <- data_in %>% 
    # select columns
    select(matches("urn|laestab")) %>%
    # remove duplicated rows
    filter(!duplicated(.)) %>%
    # check for each URN if it exists in the identify problematic parings
    mutate(
      across(contains("urn"), ~ .x %in% gias$urn_gias, .names = "urn_in_gias")
    ) %>%
    # sort data
    arrange(school_urn) %>%
    as.data.frame()
  
  # print information on whether all school urns were correct into console
  message("Note that ", sum(ids$urn_in_gias == F), " urn(s) out of ", nrow(ids), " were NOT found in GIAS.")
  
  # create id lookup table for each urn #
  # df with the following columns:
  #   urn - correct urn (either same as school urn or replaced with correct urn for that school using urn-laestab mapping with GIAS)
  #   school_urn - initial urn reported in the data
  #   laestab - laestab from GIAS
  #   school - establishment_name from gias
  id_lookup <- ids %>%
    # FIX URNs #
    # add correct urn numbers for urns without a match
    # mapping between urn and laestab for all incorrect urns
    # note: urn_gias will only be added if school_urn did not exist in the data, else urn_gias is NA
    left_join(., 
              gias[gias$laestab %in% ids$school_laestab[ids$urn_in_gias == F], c("laestab", "urn_gias")],
              join_by(school_laestab == laestab)
    ) %>% 
    mutate(
      # combine both urn variables into one with the correct URN numbers
      urn = ifelse(urn_in_gias, school_urn, urn_gias)
    ) %>%
    # FIX LAESTABS #
    left_join(., # get the correct laestab for each urn
              gias, join_by(urn == urn_gias)) %>%
    # check if laestabs are correct #
    mutate(correct_school_laestab = school_laestab == laestab) %>%
    # select columns
    select(urn, school_urn, laestab, school) %>%
    # remove duplicates
    filter(!duplicated(.)) %>%
    as.data.frame()
  
  # Return both the lookup and the modified data
  return(list(
    lookup = id_lookup,
    modified_data = data_in
  ))
}

cleanup_data <- function(data_in = df) {
  
  # Get the original dataset name
  original_dataset_name <- deparse(substitute(data_in))
  
  # create id lookup table for each urn, passing the original name
  result <- create_urn_laestab_lookup(data_in = data_in, 
                                      original_name = original_dataset_name)
  
  # Extract both the lookup and the modified data
  id_lookup <- result$lookup
  data_in <- result$modified_data  # This has the renamed columns!
  
  old_name <- "school_urn"
  new_name <- paste0("urn_", original_dataset_name)
  
  # fix id information in input data
  data_in <- data_in %>% 
    # add correct ids
    full_join(id_lookup, .) %>%
    # rename column
    rename(!!new_name := !!old_name) %>%
    # drop school_name and school_laestab
    select(-c(school_laestab)) %>%
    # sort data
    arrange(laestab, time_period) %>%
    # remove schools with more than one entry per year
    group_by(time_period, urn) %>%
    mutate(n = n()) %>%
    ungroup() %>%
    filter(n == 1) %>%
    select(-n) %>%
    as.data.frame()
  
  if ("school_name" %in% names(data_in)) {
    data_in$school_name <- NULL
  }
  
  return(data_in)
}

# Function to review column lookup table mappings
review_lookup_mappings <- function(lookup_table = column_lookup) {
  cat("=== COLUMN LOOKUP TABLE REVIEW ===\n\n")
  
  for (i in 1:nrow(lookup_table)) {
    standard <- lookup_table$standard_name[i]
    variations <- lookup_table$variations[[i]]
    
    cat(sprintf("Standard Name %d of %d:\n", i, nrow(lookup_table)))
    cat("Standard: ", standard, "\n")
    cat("Variations (", length(variations), "):\n")
    
    for (j in 1:length(variations)) {
      cat("  ", j, ". ", variations[j], "\n", sep = "")
    }
    cat("\n", paste(rep("-", 80), collapse = ""), "\n\n")
  }
}

# Create the reverse lookup function
#  transforms column lookup table from its current structure into a format 
# that's optimized for fast column name standardisation.
create_reverse_lookup <- function(lookup_table) {
  reverse_lookup <- list()
  
  for (i in 1:nrow(lookup_table)) {
    standard <- lookup_table$standard_name[i]
    variations <- lookup_table$variations[[i]]
    
    for (var in variations) {
      reverse_lookup[[var]] <- standard
    }
  }
  
  return(reverse_lookup)
}

# Enhanced function that only keeps columns included in the lookup table
# core data pipeline processor: 
## 1. Column Filtering & Selection
## 2. Column Name Standardisation
## 3. Duplicate Detection & Handling
## 4. Quality Control & Reporting
standardise_column_names <- function(df, lookup = reverse_lookup) {
  current_names <- names(df)
  
  # Get all possible column names that are in the lookup (variations that can be mapped)
  lookup_columns <- names(lookup)
  
  # Identify which columns from the dataframe are in the lookup
  columns_to_keep <- current_names[current_names %in% lookup_columns]
  
  if (length(columns_to_keep) == 0) {
    warning("No columns found that match the lookup table")
    return(df[, FALSE])  # Return empty dataframe
  }
  
  # Filter dataframe to only keep columns that are in the lookup
  df_filtered <- df[, columns_to_keep, drop = FALSE]
  
  # Now standardize the column names
  new_names <- names(df_filtered)
  for (i in 1:length(new_names)) {
    old_name <- new_names[i]
    if (old_name %in% names(lookup)) {
      new_names[i] <- lookup[[old_name]]
    }
  }
  
  # Check for duplicates BEFORE renaming
  if (any(duplicated(new_names))) {
    duplicate_names <- new_names[duplicated(new_names)]
    cat("WARNING: The following duplicate column names would be created:\n")
    print(duplicate_names)
    
    # Show which original columns are causing the duplicates
    for (dup_name in unique(duplicate_names)) {
      original_cols <- names(df_filtered)[new_names == dup_name]
      cat(paste("Columns mapping to", dup_name, ":", paste(original_cols, collapse = ", "), "\n"))
    }
    
    # Handle duplicates by keeping only the first occurrence and renaming others
    for (dup_name in unique(duplicate_names)) {
      dup_indices <- which(new_names == dup_name)
      if (length(dup_indices) > 1) {
        # Keep the first one, modify the others
        for (j in 2:length(dup_indices)) {
          new_names[dup_indices[j]] <- paste0(new_names[dup_indices[j]], "_", j-1)
        }
      }
    }
  }
  
  # Rename columns
  names(df_filtered) <- new_names
  
  # Report what happened
  original_count <- length(current_names)
  kept_count <- length(columns_to_keep)
  dropped_count <- original_count - kept_count
  
  cat(paste("Year:", ifelse(exists("academic_year"), academic_year, "Unknown"), "\n"))
  cat(paste("Original columns:", original_count, "\n"))
  cat(paste("Columns kept (in lookup):", kept_count, "\n"))
  cat(paste("Columns dropped (not in lookup):", dropped_count, "\n"))
  
  # Report column name changes
  changes <- data.frame(
    old_name = columns_to_keep[columns_to_keep != new_names],
    new_name = new_names[columns_to_keep != new_names],
    stringsAsFactors = FALSE
  )
  
  if (nrow(changes) > 0) {
    cat("Column name changes made:\n")
    print(changes)
  } else {
    cat("No column name changes needed.\n")
  }
  
  # Show which columns were dropped (for reference)
  dropped_columns <- current_names[!current_names %in% columns_to_keep]
  if (length(dropped_columns) > 0 && length(dropped_columns) <= 10) {
    cat("Dropped columns:", paste(dropped_columns, collapse = ", "), "\n")
  } else if (length(dropped_columns) > 10) {
    cat("Dropped columns (first 10):", paste(dropped_columns[1:10], collapse = ", "), "... and", length(dropped_columns) - 10, "more\n")
  }
  
  cat("\n")
  
  return(df_filtered)
}
