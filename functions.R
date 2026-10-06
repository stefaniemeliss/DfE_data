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

# Function that converts the zip filename into a sensible folder name
infer_dir_from_filename <- function(filename, base_dir = getwd()) {
  stem <- tools::file_path_sans_ext(basename(filename))
  stem <- stringr::str_squish(stem)
  
  # Convert 2018-2019 -> 2018-19
  if (grepl("^\\d{4}-\\d{4}$", stem)) {
    start_year <- sub("^(\\d{4})-\\d{4}$", "\\1", stem)
    end_year   <- sub("^\\d{4}-(\\d{4})$", "\\1", stem)
    end_2d     <- substr(end_year, 3, 4)
    return(file.path(base_dir, paste0(start_year, "-", end_2d)))
  }
  
  # Already in academic-year format: 2018-19
  if (grepl("^\\d{4}-\\d{2}$", stem)) {
    return(file.path(base_dir, stem))
  }
  
  # Fallback: use the stem as-is
  file.path(base_dir, stem)
}

# function to download data from an URL that directly links to a file
download_data_from_url <- function(url, academic_year = NULL, parent_url = NULL) {
  
  # Defensive assignment for dir_term
  if (!exists("dir_term", inherits = TRUE)) dir_term <- character(0)
  
  headers <- c(
    `user-agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/102.0.5005.61 Safari/537.36"
  )
  
  max_attempts <- 5
  successful <- FALSE
  
  for (attempt in seq_len(max_attempts)) {
    message(sprintf("Attempt %d to download: %s", attempt, url))
    
    request <- try(
      httr::GET(url = url, httr::add_headers(.headers = headers)),
      silent = TRUE
    )
    
    if (inherits(request, "try-error")) {
      message(sprintf(
        "HTTP request failed on attempt %d. Retrying in %d seconds...",
        attempt, 2^(attempt - 1)
      ))
      Sys.sleep(2^(attempt - 1))
      next
    }
    
    status_code <- httr::status_code(request)
    
    if (status_code != 200) {
      if (status_code == 404) {
        stop(sprintf("HTTP 404 error on attempt %d: URL likely invalid.", attempt))
      } else {
        message(sprintf(
          "HTTP status %d on attempt %d. Retrying in %d seconds...",
          status_code, attempt, 2^(attempt - 1)
        ))
        Sys.sleep(2^(attempt - 1))
      }
    } else {
      successful <- TRUE
      break
    }
  }
  
  if (!successful) {
    stop(sprintf("Failed to retrieve data after %d attempts.", max_attempts))
  }
  
  # Determine filename from headers or URL
  content_disposition <- request$headers$`content-disposition`
  
  if (!is.null(content_disposition) && nzchar(content_disposition)) {
    tmp <- sub('.*filename="?([^";]+)"?.*', "\\1", content_disposition)
    tmp <- utils::URLdecode(tmp)
    tmp <- basename(tmp)
  } else {
    message("Warning: 'content-disposition' header missing. Using default filename.")
    tmp <- paste0("download_", basename(url))
  }
  
  # Historic accountability data:
  # save zip directly into dir_out, because the archive already contains year folders
  if (exists("parent_url", inherits = TRUE) &&
      grepl("historic-accountability-data", parent_url)) {
    
    dir_release <- dir_out
    
  } else {
    
    # Create / determine year directory if needed
    dir_year_missing <- !exists("dir_year") ||
      length(dir_year) == 0 ||
      is.na(dir_year) ||
      !nzchar(dir_year)
    
    if (dir_year_missing) {
      if (!is.null(academic_year) && !is.na(academic_year) && nzchar(academic_year)) {
        dir_year <- file.path(dir_out, academic_year)
      } else {
        assign_dir_year("dir_year_data", url)
        
        if (exists("dir_year_data") &&
            length(dir_year_data) > 0 &&
            !is.na(dir_year_data) &&
            nzchar(dir_year_data)) {
          dir_year <- dir_year_data
        } else {
          stop("Could not determine directory for: ", url)
        }
      }
    }
    
    # determine dir_release safely
    if (identical(dir_term, character(0)) ||
        length(dir_term) == 0 ||
        is.na(dir_term) ||
        !nzchar(dir_term)) {
      
      dir_release <- dir_year
      
    } else if (is.na(dir_year) || !nzchar(dir_year)) {
      
      dir_release <- dir_term
      
    } else if (normalizePath(dir_term, winslash = "/", mustWork = FALSE) !=
               normalizePath(dir_year, winslash = "/", mustWork = FALSE)) {
      
      dir_release <- dir_term
      
    } else {
      
      dir_release <- dir_year
    }
  }
  
  if (is.na(dir_release) || !nzchar(dir_release)) {
    stop("dir_release is invalid: ", dir_release)
  }
  
  if (!dir.exists(dir_release)) {
    dir.create(dir_release, recursive = TRUE)
  }
  
  file_name <- file.path(dir_release, tmp)
  
  message("Downloading file from URL...")
  message("\tURL: ", url)
  message("\tSaving to: ", file_name)
  
  bin <- httr::content(request, "raw")
  writeBin(bin, file_name)
  
  if (!file.exists(file_name)) {
    stop("Download failed: file was not written to disk: ", file_name)
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
      for (i in seq_len(max_retries)) {
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
        warning(
          "Failed to remove ZIP file after ", max_retries,
          " attempts. Please delete manually: ", file_name
        )
      }
      
      message("Unzipped files are saved in: ", dir_release)
      
    }, error = function(e) {
      warning("Unzip failed (", e$message, "). Keeping ZIP file for manual inspection.")
    })
  }
  
  invisible(file_name)
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
  
  # Initialise the tracking vector
  downloaded_links <- character(0)
  
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
  
  
  # Filter the links using the specified pattern
  if(grepl("historic-accountability-data", parent_url)) {
    
    # get all items from the release list
    items <- webpage %>% html_elements("li[data-testid='release-data-list-item']")
    
    # create df that contains download links as well as info on academic year etc
    release_df <- map_dfr(items, function(x) {
      title <- x %>% html_element("h4") %>% html_text2()
      desc  <- x %>% html_element("p")  %>% html_text2()
      href  <- x %>% html_element("a")  %>% html_attr("href")
      
      tibble(
        academic_year = stringr::str_remove(title, "\\s*Accountability Data$"),
        title = title,
        description = desc,
        download_url = href
      )
    })
    
    # filter using pattern_to_match
    download_links <- release_df %>%
      filter(academic_year >= pattern_to_match) %>%
      pull(download_url)
    
  } else {
    # check if there are any application/octet-stream absolute_links
    download_links <-  unique(absolute_links[grepl("/files$", absolute_links)])
    # download_links <-  unique(absolute_links[grepl("/files$|content.explore|data-catalogue", absolute_links)])
    # download_links <-  unique(absolute_links[grepl("/files", absolute_links)])
    
  }
  

  if (identical(download_links, character(0)) == F) {
    cat("\nFound download links on parent URL...\n")
    cat("\t", download_links, sep = "\n\t")
    cat("\n")
    # if so, download
    invisible(sapply(download_links, function(x) download_data_from_url(url = x, parent_url = parent_url)))
    
    # finish execution
    if(grepl("historic-accountability-data", parent_url)) return(invisible(NULL))
    
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
      has_download_zip <- any(grepl("Download all data", button_texts, ignore.case = TRUE) & grepl("ZIP", button_texts, ignore.case = TRUE))
      
      if (has_download_zip) {
        cat("\nFound 'Download all data (ZIP)' button. Attempting automated download...\n")
        download_data_via_button_chromote(release_url, download_dir = dir_release)
      } else {
        
        # --- Fallback: Download from links as before ---
        
        # Extract all <a> tags and their attributes
        link_nodes <- html_nodes(webpage, "a")
        hrefs <- html_attr(link_nodes, "href")
        texts <- html_text(link_nodes)
        
        # --- Robust ZIP link extraction ---
        zip_link <- hrefs[grepl("Download all data", texts, ignore.case = TRUE) & grepl("ZIP", texts, ignore.case = TRUE)]
        if (length(zip_link) > 0 && !is.na(zip_link) && !(zip_link %in% downloaded_links)) {
          cat("\nFound 'Download all data (ZIP)' link. Attempting download...\n")
          tryCatch({
            download_data_from_url(url = zip_link, parent_url = parent_url)
            downloaded_links <- c(downloaded_links, zip_link)
          }, error = function(e) {
            cat("Failed to download URL:", zip_link, "\nError message:", e$message, "\n")
          })
        }
        
        # Extract all the links from the webpage
        links <- hrefs  # links are the href attribute of all <a> tags
        
        # Apply function to resolve relative URLs to all links
        absolute_links <- sapply(links, function(link) {
          resolve_url(parent_url, link)
        })
        
        # Filter the download links (e.g., links ending with .pdf)
        download_links <- absolute_links[grepl("\\.[a-zA-Z]+$|/files/", absolute_links)]
        download_links <- download_links[!grepl(".uk$", download_links)]
        download_links <- download_links[!grepl(".com$", download_links)]
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
          for (dl in download_links) {
            if (dl %in% downloaded_links) {
              cat("Skipping already downloaded link:", dl, "\n")
              next
            }
            tryCatch({
              download_data_from_url(url = dl, parent_url = parent_url)
              downloaded_links <- c(downloaded_links, dl)
            }, error = function(e) {
              cat("Failed to download URL:", dl, "\nError message:", e$message, "\n")
            })
          }
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
  # select rows and columns
  tmp <- data_in[data_in[, col_to_filter] %in% filter, c(identifier_columns, var_nrd, var_rd)]
  
  tmp <- tmp[!is.na(tmp[, var_nrd]) & !is.na(tmp[, var_rd]), ]
  
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


fix_roundings <- function(var_nrd = "variable_not_rounded", var_rd = "variable_rounded",
                          new_var = "",
                          identifier_columns = "id_cols",
                          col_to_filter = "col_name",
                          filter = vector(),
                          rounding_factor = 5,
                          data_in = df_in) {
  # Copy input data to avoid modifying in place
  tmp <- data_in[, c(identifier_columns, var_nrd, var_rd)]
  
  # Compute difference in raw values
  tmp$diff <- tmp[[var_nrd]] - tmp[[var_rd]]
  
  # Round variable currently not rounded
  tmp$rd <- round(tmp[[var_nrd]] / rounding_factor) * rounding_factor
  
  # Replace any instances of rounded values with unrounded values (for all rows)
  tmp$test <- ifelse(!is.na(tmp[[var_nrd]]) & tmp[[var_nrd]] != 0 & tmp[[col_to_filter]] %in% filter, tmp[[var_nrd]], tmp[[var_rd]])
  
  # Compute diff after replacing rounded values with unrounded values
  tmp$diff2 <- tmp[[var_nrd]] - tmp$test
  
  # Assign the new variable as requested
  if (new_var != "") {
    tmp[[new_var]] <- ifelse(tmp[[col_to_filter]] %in% filter, tmp[[var_nrd]], tmp[[var_rd]])
  } else {
    tmp[[paste0(var_rd, "_orig")]] <- tmp[[var_rd]] # copy original unrounded values
    tmp[[var_rd]] <- ifelse(tmp[[col_to_filter]] %in% filter, tmp[[var_nrd]], tmp[[var_rd]])
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
      across(contains("urn"), ~ .x %in% gias$URN, .names = "urn_in_gias"),
      across(contains("laestab"), ~ .x %in% gias$LAESTAB, .names = "lae_in_gias")
    ) %>%
    # sort data
    arrange(school_urn) %>%
    as.data.frame()
  
  # print information on whether all school urns and laestabs were correct into console
  if (sum(ids$urn_in_gias == F) != 0) message("Note that ", sum(ids$urn_in_gias == F), " URN(s) out of ", nrow(ids), " were NOT found in GIAS data.")
  if (sum(ids$lae_in_gias == F) != 0) message("Note that ", sum(ids$lae_in_gias == F), " LAESTAB(s) out of ", nrow(ids), " were NOT found in GIAS data.")
  
  # create id lookup table for each urn #
  
  # one of four scenarios
  #   1. urn and lae both match --> !is.na(URN) & !is.na(LAESTAB)
  #   2. urn matches but lae does not --> !is.na(URN) & is.na(LAESTAB)
  #   3. lae matches but urn does not --> is.na(URN) & !is.na(LAESTAB)
  #   4. neither matches --> is.na(URN) & is.na(LAESTAB)
  
  id_lookup <- ids %>%
    select(!contains("_in_")) %>%
    # add GIAS URNs and matching LAESTABs for all urns #
    left_join(., gias %>%
                mutate(urn = URN),
              join_by(school_urn == urn)
    ) %>%
    # FIX URNs #
    #   add correct urn numbers for urns without a match
    #   mapping between urn and laestab for all incorrect urns
    #   note: urn_gias will only be added if school_urn did not exist in the data, else urn_gias is NA
    left_join(., gias %>%
                filter(LAESTAB %in% ids$school_laestab[!ids$urn_in_gias]) %>%
                rename(tmp = URN),
              join_by(school_laestab == LAESTAB, ESTAB_NAME)
    ) %>%
    mutate(URN = if_else(!is.na(tmp), tmp, URN)) %>%
    select(-tmp)
  
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
  
  old_name1 <- "school_urn"
  new_name1 <- paste0("urn_", original_dataset_name)
  old_name2 <- "school_laestab"
  new_name2 <- paste0("laestab_", original_dataset_name)
  
  # fix id information in input data
  data_in <- data_in %>% 
    # add correct ids
    full_join(id_lookup, .) %>%
    # rename column
    rename(!!new_name1 := !!old_name1, 
           !!new_name2 := !!old_name2) %>%
    # sort data
    arrange(LAESTAB, time_period) %>%
    # remove schools with more than one entry per year
    group_by(time_period, URN) %>%
    filter(n() == 1) %>%
    ungroup() %>%
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


# Create URN / LAESTAB lookup using Per Annum GIAS
create_urn_laestab_lookup_pa <- function(data_in,
                                         gias,
                                         time_period,
                                         original_name = NULL) {
  
  #' Create URN / LAESTAB lookup and optionally expand reference GIAS
  #'
  #' Purpose:
  #'   - Build a lookup table that maps each unique (school_urn, school_laestab)
  #'     pair in `data_in` to a (hopefully) correct URN, using GIAS as the
  #'     reference.
  #'   - Optionally expand the reference GIAS (`gias_ref`) with the closest
  #'     available rows for:
  #'       * URNs that exist in some GIAS year but not in the target year.
  #'       * LAESTABs for URNs that do not exist in any GIAS year, when the
  #'         LAESTAB itself exists in GIAS.
  #'
  #' Intended logic:
  #'   1. For all rows where `school_urn` exists in GIAS (school_urn == URN),
  #'      bring in the corresponding LAESTAB (and other GIAS fields).
  #'   2. For rows where `school_urn` does NOT occur in any GIAS year,
  #'      use `school_laestab` to match LAESTAB in GIAS and recover the correct URN.
  #'
  #' Expansion rules:
  #'   - URN expansion:
  #'       * For URNs that are missing from the reference year but exist in
  #'         some other GIAS year, add the closest year's row to `gias_ref`.
  #'   - LAESTAB expansion:
  #'       * Only when:
  #'           - URN not in any GIAS year (!urn_in_gias),
  #'           - LAESTAB not in the reference year (!lae_in_ref),
  #'           - LAESTAB exists somewhere in the full GIAS (lae_in_gias).
  #'       * For such LAESTABs, add the closest year's row to `gias_ref`.
  #'
  #' Lookup construction:
  #'   - Start from one row per unique (school_urn, school_laestab).
  #'   - First join: match by URN (school_urn == URN) to add GIAS fields
  #'     (including LAESTAB) for schools that exist in the reference GIAS.
  #'   - Second join: restricted to LAESTABs belonging to URNs that are missing
  #'     from all GIAS years; join on (LAESTAB, Data_Download_Date, School_Name)
  #'     to recover the correct URN where possible.
  #'   - Final URN is:
  #'       * The URN from the first join if present,
  #'       * Otherwise the URN recovered via LAESTAB (if any).
  #'
  #' Safeguards:
  #'   - Checks that URN is unique in `gias_ref`.
  #'   - Checks that (LAESTAB, Data_Download_Date, School_Name) is unique for
  #'     LAESTABs associated with URNs that are missing from all GIAS years.
  #'   - Stops if the final lookup does not have exactly one row per unique
  #'     (school_urn, school_laestab) pair.
  #'
  #' @param data_in       Data frame with at least `school_urn` and `school_laestab`.
  #'                      May also have `urn` / `laestab`, which will be renamed.
  #' @param gias          Full GIAS data with at least `Academic_Year`, `URN`, `LAESTAB`,
  #'                      plus `Data_Download_Date` and `School_Name` for the second join.
  #' @param time_period   Target academic year (e.g. 202223). Values < 202021 are
  #'                      treated as 202021.
  #' @param original_name Optional name for the dataset (used only in the output list).
  #'
  #' @return A list with:
  #'   \describe{
  #'     \item{dataset_name}{Character name for the input dataset.}
  #'     \item{lookup}{Data frame with one row per unique (school_urn, school_laestab)
  #'                   and a resolved `URN` column.}
  #'     \item{modified_data}{The original `data_in` (with possible column renames).}
  #'     \item{gias_ref}{The expanded reference GIAS used for the lookup.}
  #'   }
  
  
  # Optional dataset name (only used if you want it in the output)
  dataset_name <- if (is.null(original_name)) {
    deparse(substitute(data_in))
  } else {
    original_name
  }
  
  # Standardise input column names
  if ("urn" %in% names(data_in) && !"school_urn" %in% names(data_in)) {
    data_in <- data_in %>% dplyr::rename(school_urn = urn)
  }
  
  if ("laestab" %in% names(data_in) && !"school_laestab" %in% names(data_in)) {
    data_in <- data_in %>% dplyr::rename(school_laestab = laestab)
  }
  
  # Check required columns exist
  required_data_cols <- c("school_urn", "school_laestab")
  missing_data_cols <- setdiff(required_data_cols, names(data_in))
  if (length(missing_data_cols) > 0) {
    stop(
      "data_in is missing required column(s): ",
      paste(missing_data_cols, collapse = ", ")
    )
  }
  
  required_gias_cols <- c("Academic_Year", "URN", "LAESTAB")
  missing_gias_cols <- setdiff(required_gias_cols, names(gias))
  if (length(missing_gias_cols) > 0) {
    stop(
      "gias is missing required column(s): ",
      paste(missing_gias_cols, collapse = ", ")
    )
  }
  
  # Reference GIAS for the requested academic year
  if (time_period < 202021) {
    gias_ref <- gias %>%
      dplyr::filter(Academic_Year == 202021)
  } else {
    gias_ref <- gias %>%
      dplyr::filter(Academic_Year == time_period)
  } 
  
  # Unique ID pairs from input
  ids <- data_in %>%
    dplyr::select(school_urn, school_laestab) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      urn_in_ref = school_urn %in% gias_ref$URN,
      lae_in_ref = school_laestab %in% gias_ref$LAESTAB,
      urn_in_gias = school_urn %in% gias$URN,
      lae_in_gias = school_laestab %in% gias$LAESTAB
    ) %>%
    dplyr::arrange(school_urn) %>%
    as.data.frame()
  
  # Messages
  if (any(!ids$urn_in_ref)) {
    message("Note that ", sum(!ids$urn_in_ref), " URN(s) out of ", nrow(ids),
            " were not found in reference GIAS data (", time_period, ").")
  }
  if (any(!ids$urn_in_gias)) {
    message("Note that ", sum(!ids$urn_in_gias), " URN(s) out of ", nrow(ids),
            " were not found in any GIAS data.")
  }
  
  # Expand reference GIAS with closest available row for URNs
  missing_urns_in_ref <- ids$school_urn[!ids$urn_in_ref & ids$urn_in_gias]
  
  if (length(missing_urns_in_ref) > 0) {
    gias_ref_extra_urn <- gias %>%
      dplyr::filter(URN %in% missing_urns_in_ref) %>%
      dplyr::mutate(time_diff = abs(Academic_Year - time_period)) %>%
      dplyr::group_by(URN) %>%
      dplyr::slice_min(time_diff, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::select(-time_diff)
    
    gias_ref <- dplyr::bind_rows(gias_ref, gias_ref_extra_urn) %>%
      dplyr::distinct()
  }
  
  # Update ids
  ids <- ids %>%
    dplyr::mutate(
      urn_in_ref = school_urn %in% gias_ref$URN,
      lae_in_ref = school_laestab %in% gias_ref$LAESTAB
    ) %>%
    dplyr::arrange(school_urn) %>%
    as.data.frame()
  
  # Expand reference GIAS with closest available row for LAESTABs
  # ONLY when:
  #   - URN not in any GIAS
  #   - LAESTAB not in reference year
  #   - LAESTAB exists somewhere in full GIAS
  missing_laes_in_ref <- ids$school_laestab[
    !ids$urn_in_gias & !ids$lae_in_ref & ids$lae_in_gias
  ]
  
  if (length(missing_laes_in_ref) > 0) {
    gias_ref_extra_lae <- gias %>%
      dplyr::filter(LAESTAB %in% missing_laes_in_ref) %>%
      dplyr::mutate(time_diff = abs(Academic_Year - time_period)) %>%
      dplyr::group_by(LAESTAB) %>%
      dplyr::slice_min(time_diff, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::select(-time_diff)
    
    gias_ref <- dplyr::bind_rows(gias_ref, gias_ref_extra_lae) %>%
      dplyr::distinct()
  }
  
  # Check uniqueness
  if (gias_ref %>% count(URN) %>% filter(n > 1) %>% nrow() > 0) {
    message("WARNING: URN is NOT unique in gias_ref")
  }
  
  # Only problematic for URNs not found in any GIAS year
  bad_lae <- ids$school_laestab[!ids$urn_in_gias]
  if (gias_ref %>%
      filter(LAESTAB %in% bad_lae) %>%
      count(LAESTAB, Data_Download_Date, School_Name) %>%
      filter(n > 1) %>%
      nrow() > 0) {
    message("WARNING: (LAESTAB, Data_Download_Date, School_Name) not unique for bad-URN LAESTABs")
  }  
  
  # Build lookup table: one row per unique (school_urn, school_laestab)
  #
  # Intended logic:
  #   1. For all rows where school_urn exists in GIAS (school_urn == URN),
  #      bring in the corresponding LAESTAB (and other GIAS fields).
  #   2. For rows where school_urn does NOT occur in any GIAS year,
  #      use school_laestab to match LAESTAB in GIAS and recover the correct URN.
  
  id_lookup <- ids %>%
    dplyr::select(!dplyr::contains("_in_")) %>%
    
    # 1) Match by URN (school_urn == URN) to add the GIAS LAESTAB (and other fields)
    #    for all schools that exist in the reference GIAS.
    dplyr::left_join(
      .,
      gias_ref %>%
        dplyr::select(-Academic_Year) %>%
        dplyr::mutate(urn = URN),
      dplyr::join_by(school_urn == urn)
    ) %>%
    
    # 2) For schools where school_urn does NOT occur in any GIAS year,
    #    use school_laestab to match LAESTAB in GIAS and recover the correct URN.
    #    We restrict to LAESTABs belonging to URNs that are missing from all GIAS,
    #    then join on (LAESTAB, Data_Download_Date, School_Name). The matched URN
    #    comes in as `tmp` and overwrites URN where available.
    dplyr::left_join(
      .,
      gias_ref %>%
        dplyr::filter(LAESTAB %in% ids$school_laestab[!ids$urn_in_gias]) %>%
        dplyr::rename(tmp = URN) %>%
        dplyr::select(-Academic_Year),
      dplyr::join_by(school_laestab == LAESTAB, Data_Download_Date, School_Name)
    ) %>%
    dplyr::mutate(URN = dplyr::if_else(!is.na(tmp), tmp, URN)) %>%
    dplyr::select(-tmp)
  
  # Safety check: lookup should have same number of rows as ids
  if (nrow(id_lookup) != nrow(ids)) {
    stop(
      "Internal error: id_lookup has ", nrow(id_lookup),
      " rows but should have ", nrow(ids), " rows."
    )
  }
  
  return(list(
    dataset_name = dataset_name,
    lookup = id_lookup,
    modified_data = data_in,
    gias_ref = gias_ref
  ))
}


# Clean up education data using Per Annum URN/LAESTAB lookup
cleanup_data_pa <- function(data_in = df, gias, time_period) {
  
  #' Clean up education data using URN/LAESTAB lookup
  #'
  #' Purpose:
  #'   - Use `create_urn_laestab_lookup_pa()` to resolve correct URNs and LAESTABs
  #'     for a school-level dataset.
  #'   - Merge the resolved IDs back into the original data.
  #'   - Rename ID columns to include the original dataset name as a suffix
  #'     (e.g. `urn_swc`, `laestab_swc`).
  #'   - Remove duplicate school entries per (time_period, URN).
  #'   - Drop `school_name` if present (to avoid redundancy with GIAS-based names).
  #'
  #' Intended logic:
  #'   1. Call `create_urn_laestab_lookup_pa()` to:
  #'      - Standardise ID columns to `school_urn` and `school_laestab`.
  #'      - Expand reference GIAS where appropriate.
  #'      - Build a lookup with one row per unique (school_urn, school_laestab)
  #'        and a resolved `URN`.
  #'   2. Join the lookup back to the (possibly renamed) input data via a
  #'      `full_join`, so that:
  #'      - All rows from both tables are kept.
  #'      - Resolved URNs and GIAS fields are attached where possible.
  #'   3. Rename ID columns:
  #'      - `school_urn` → `urn_<original_dataset_name>`
  #'      - `school_laestab` → `laestab_<original_dataset_name>`
  #'   4. Sort by `LAESTAB` and `time_period`.
  #'   5. Remove duplicate school entries:
  #'      - Within each (time_period, URN) group, keep only groups with a single
  #'        row (i.e. drop schools that have more than one entry per year).
  #'   6. Drop `school_name` if it exists (optional cleanup step).
  #'
  #' @param data_in     Data frame with school-level data. Must contain at least
  #'                    `school_urn` or `urn`, and `school_laestab` or `laestab`,
  #'                    plus `time_period` and `URN` (or columns that will become
  #'                    these after lookup).
  #' @param gias        Full GIAS data as required by `create_urn_laestab_lookup_pa()`.
  #' @param time_period Target academic year (passed to the lookup function).
  #'
  #' @return A cleaned data frame with:
  #'   - Resolved URN and LAESTAB columns named `urn_<dataset>` and `laestab_<dataset>`.
  #'   - Duplicates per (time_period, URN) removed.
  #'   - `school_name` dropped if it existed.
  
  
  # Get the original dataset name (for column naming)
  original_dataset_name <- deparse(substitute(data_in))
  
  # Create ID lookup table for each URN, passing the original dataset name
  # This also standardises column names to school_urn / school_laestab and
  # optionally expands gias_ref with closest available rows for missing URNs/LAESTABs.
  result <- create_urn_laestab_lookup_pa(
    data_in = data_in,
    original_name = original_dataset_name,
    gias = gias,
    time_period = time_period
  )
  
  # Extract both the lookup and the modified data
  id_lookup <- result$lookup
  data_in <- result$modified_data  # This has the renamed columns (school_urn, school_laestab)
  
  old_name1 <- "school_urn"
  new_name1 <- paste0("urn_", original_dataset_name)
  old_name2 <- "school_laestab"
  new_name2 <- paste0("laestab_", original_dataset_name)
  
  # Fix ID information in input data
  data_in <- data_in %>%
    # Add correct IDs by joining the lookup to the data.
    # Using full_join keeps all rows from both tables; adjust if you prefer
    # left_join(data_in, id_lookup, by = c("school_urn", "school_laestab")).
    dplyr::full_join(id_lookup, .) %>%
    
    # Rename ID columns to include the original dataset name as a suffix
    dplyr::rename(
      !!new_name1 := !!old_name1,
      !!new_name2 := !!old_name2
    ) %>%
    
    # Sort data by LAESTAB and time_period
    dplyr::arrange(LAESTAB, time_period) %>%
    
    # Remove schools with more than one entry per (time_period, URN)
    # This keeps only groups where there is exactly one row per year.
    dplyr::group_by(time_period, URN) %>%
    dplyr::filter(n() == 1) %>%
    dplyr::ungroup() %>%
    as.data.frame()
  
  # Remove school_name if present (optional cleanup; avoids redundancy with GIAS)
  if ("school_name" %in% names(data_in)) {
    data_in$school_name <- NULL
  }
  
  return(data_in)
}
