#######################################
#### Get Information about Schools ####
#######################################

# Get Information about Schools (GIAS) is the Department for Education’s register 
# for several organisation types and where information on other organisations is 
# recorded and maintained.

# GIAS is used by the department and key partners to contact establishments, 
# update systems, perform analysis and inform policy decisions, some of which 
# carry funding implications.

# Establishment, federation, academy trust and approved local authority users 
# can access their records by signing into their DfE Sign-in (DSI) account.

# ‘Get Information about Schools’ is a comprehensive database of educational establishments in England. 
# It contains data on around 65,000 establishments, including academies, maintained schools and children’s centres.

# Get Information about Schools currently contains over 250 data fields for each establishment, 
# and this number is expected to grow.

# **SEARCH**: This is a register of schools and colleges in England. 
# You can search for and download information on establishments, establishment groups 
# (such as a local authority, trust or federation) or governors. 
# Schools, local authorities and academy trusts can also update details by signing in to their DfE Sign-in account.

# **DOWNLOAD**: This data is updated daily. The data fields and the order of the data fields can change in these downloads. 

# Empty the workspace in the global environment except for this function
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

# Create subfolders if they do not exist
if (!dir.exists(dir_data)) {
  dir.create(dir_data)
}

if (!dir.exists(dir_misc)) {
  dir.create(dir_misc)
}

# get file stem name
file_stem <- get_file_stem()

# Load necessary libraries
library(data.table)

# Source external functions
# devtools::source_url("https://github.com/stefaniemeliss/edu_stats/blob/main/functions.R?raw=TRUE")


#### source: https://get-information-schools.service.gov.uk/Search ####

# date downloaded 26/06/2025
stem <- "results_gias_search_core"
# All establishments --> Download these search results --> Core set of data + Links
stem <- "results_gias_search_full"
# All establishments --> Download these search results --> Full set of data + Links
res <- fread(file.path(dir_misc, stem, "results.csv"), fill = Inf, header = TRUE, sep = ",")


# fix col names
names(res) <- tolower(names(res))
names(res) <- gsub("(", "", names(res), fixed = T)
names(res) <- gsub(")", "", names(res), fixed = T)
names(res) <- gsub(" ", "_", names(res), fixed = T)

# Convert encoding using iconv
res$establishmentname <- iconv(res$establishmentname, from = "latin1", to = "UTF-8")

# Linked establishments #

# identify index of column named linked_establishments
idx_start <- as.numeric(which(names(res) == "linked_establishments"))

# identify index of last column (also contains info on linked establishments)
idx_stop <- as.numeric(ncol(res))

# overwrite column names
names(res)[idx_start:idx_stop] <- sprintf("linked_establishments_%02d", 1:(idx_stop - idx_start + 1))

# concatenate info on linked establishments
res[, links := do.call(paste, c(.SD, sep = " ")), .SDcols = idx_start:idx_stop]

# LA ESTAB #

# Ensure laestab_number has leading zeros and concatenate with dfe_number
res[, establishmentnumber_str := ifelse(!is.na(establishmentnumber), sprintf("%04d", establishmentnumber), NA)]

# combine LA code with estab number if LA code is not zero and estab number is not NA
res[, laestab := ifelse(!is.na(establishmentnumber) & la_code != 0, as.numeric(paste0(la_code, establishmentnumber_str)), NA)]


# format dates #
res[, opendate := ifelse(opendate == "", NA_character_, opendate)]
res[, opendate := as.Date(opendate, format = "%d-%m-%Y")]
res[, closedate := ifelse(closedate == "", NA_character_, closedate)]
res[, closedate := as.Date(closedate, format = "%d-%m-%Y")]

# make all empty cells NA_character_
res[, (names(res)[sapply(res, is.character)]) := lapply(.SD, function(x) { ifelse(x == "", NA_character_, x)}), .SDcols = sapply(res, is.character)]



# fix religion #

# Define a pattern to match Christian denominations
christian_patterns <- c("Church", "Catholic", "Christian", "Anglican", "Methodist", "Adventist", "Evangelical", "Protestant", "Moravian", "Quaker", "Baptist")
# Create a regex pattern
pattern <- paste(christian_patterns, collapse = "|")

# Identify Christian denominations
res[, religiouscharacter_christian := grepl(pattern, religiouscharacter_name, ignore.case = TRUE)]

# fix urbanicity
res[, urbanicity := ifelse(grepl("Urban|urban", urbanrural_name), "Urban",
                          ifelse(grepl("Rural|rural", urbanrural_name), "Rural", NA))]

# fix gender #
res[, sex_students := fifelse(res[, gender_name] == "Girls" | res[, gender_name] == "Boys", "Single-sex",
                             fifelse(res[, gender_name] == "Mixed", "Co-ed", res[, gender_name]))]




# select columns
out <- res[, .(laestab, urn, la_code, establishmentnumber, establishmentname,
              street, postcode, town, gor_name, la_name, districtadministrative_name, administrativeward_name, parliamentaryconstituency_name, #lat, long,
              typeofestablishment_name, 
              establishmentstatus_name, opendate, reasonestablishmentopened_name,
              closedate, reasonestablishmentclosed_name,
              phaseofeducation_name, statutorylowage, statutoryhighage,
              boarders_name, nurseryprovision_name, officialsixthform_name,
              gender_name, sex_students, religiouscharacter_name, religiouscharacter_christian, diocese_name,
              admissionspolicy_name, urbanrural_name, urbanicity,
              trustschoolflag_name, trusts_name, links
)]

# sort
setorder(out, urn)

# save file
fwrite(out, file = file.path(dir_data, "data_gias_search.csv"), bom = T)


#### source: https://get-information-schools.service.gov.uk/Downloads ####
stem <- "extract_gias_download"
# date downloaded 26/06/2025
# Establishment downloads (select all)
# Establishment fields CSV
# Establishment links CSV

# Establishment groups
# All group data (select all)
# SAT and MAT membership history CSV
# All group records.csv
# All group links records.csv
# All group records with links.zip


# process data #

# Establishment links
links <- fread(file = file.path(dir_misc, stem, "links_edubasealldata20250626.csv"))
names(links) <- tolower(names(links))
links$linkname <- iconv(links$linkname, from = "latin1", to = "UTF-8")

# Establishment fields
fields <- fread(file = file.path(dir_misc, stem, "edubasealldata20250626.csv"))

# fix col names
names(fields) <- tolower(names(fields))
names(fields) <- gsub("(", "", names(fields), fixed = T)
names(fields) <- gsub(")", "", names(fields), fixed = T)
names(fields) <- gsub(" ", "_", names(fields), fixed = T)

fields$establishmentname <- iconv(fields$establishmentname, from = "latin1", to = "UTF-8")

# select cols
dt <- fields[, .SD, .SDcols = 
               grepl("urn|la_code|establishmentn|statut|boarders|nurs|sixth|gender|relig|dioc|admiss|trust|urban|country|street|town|postcode|open|close|typeofe|uprn|phase|status|gor|parlia|district|ward|la_name", names(fields))]

# # Subset data
# dt <- dt[la_code != 0 & # no data from schools that are not affiliated with a LA
#            !is.na(establishmentnumber) & # or don't have an estab number
#            !country_name %in% c("Gibraltar", "Jersey") & # or are in Jersey or Gibralta
#            !grepl("Welsh|Further education|Miscellaneous|Higher education institutions", typeofestablishment_name) & # only schools
#            !grepl("Wales|Not Applicable", gor_name)] # or Wales

# Ensure laestab_number has leading zeros and concatenate with dfe_number
fields[, establishmentnumber_str := ifelse(!is.na(establishmentnumber), sprintf("%04d", establishmentnumber), NA)]
fields[, laestab := ifelse(!is.na(establishmentnumber), as.numeric(paste0(la_code, establishmentnumber_str)), NA)]

# extract key
key <- fields[, .(laestab, urn, establishmentname)]

# subset links data so that it only includes relevant URNs
links <- links[urn %in% key[, urn]]
# process date
links[, linkestablisheddate := ifelse(linkestablisheddate == "", NA_character_, linkestablisheddate)]
links[, linkestablisheddate := as.Date(linkestablisheddate, format = "%d-%m-%Y")]

# add link data
dt <- merge(fields, links, by = "urn", all.x = T)

# format dates
dt[, opendate := ifelse(opendate == "", NA_character_, opendate)]
dt[, opendate := as.Date(opendate, format = "%d-%m-%Y")]
dt[, closedate := ifelse(closedate == "", NA_character_, closedate)]
dt[, closedate := as.Date(closedate, format = "%d-%m-%Y")]

# replace 99[99] with NA
dt[, diocese_code := ifelse(diocese_code == "0000" | diocese_code == "9999", NA_character_, diocese_code)]
dt[, religiouscharacter_code := ifelse(religiouscharacter_code == 99, NA, religiouscharacter_code)]
dt[, previousla_code := ifelse(previousla_code == 999 | previousla_code == 0, NA, previousla_code)]
dt[, urbanrural_code := ifelse(urbanrural_code == "99", NA_character_, urbanrural_code)]
dt[, reasonestablishmentopened_code := ifelse(reasonestablishmentopened_code == 99, NA, reasonestablishmentopened_code)]
dt[, reasonestablishmentclosed_code := ifelse(reasonestablishmentclosed_code == 99, NA, reasonestablishmentclosed_code)]

# make all empty cells NA_character_
dt[, (names(dt)[sapply(dt, is.character)]) := lapply(.SD, function(x) { ifelse(x == "", NA_character_, x)}), .SDcols = sapply(dt, is.character)]

# fix religion #
# Define a pattern to match Christian denominations
christian_patterns <- c("Church", "Catholic", "Christian", "Anglican", "Methodist", "Adventist", "Evangelical", "Protestant", "Moravian", "Quaker", "Baptist")
# Create a regex pattern
pattern <- paste(christian_patterns, collapse = "|")

# Identify Christian denominations
dt[, religiouscharacter_christian := grepl(pattern, religiouscharacter_name, ignore.case = TRUE)]

out <- dt[, .(laestab, urn, la_code, establishmentnumber, establishmentname,
              street, postcode, town, gor_name, la_name, districtadministrative_name, administrativeward_name, parliamentaryconstituency_name, #lat, long,
              typeofestablishment_code, typeofestablishment_name, 
              establishmentstatus_name, opendate, reasonestablishmentopened_code, reasonestablishmentopened_name,
              closedate, reasonestablishmentclosed_code, reasonestablishmentclosed_name,
              phaseofeducation_code, phaseofeducation_name, statutorylowage, statutoryhighage,
              boarders_name, nurseryprovision_name, officialsixthform_name,
              gender_name, religiouscharacter_name, religiouscharacter_christian, diocese_name,
              admissionspolicy_name, urbanrural_name,
              trustschoolflag_name, trusts_code, trusts_name,
              previousla_code, previousestablishmentnumber,
              linkurn, linkname, linktype, linkestablisheddate
)]
# sort
setorder(out, urn)

# save file
fwrite(out, file = file.path(dir_data, "data_gias_download.csv"), bom = T)

# Establishment groups
groups <- fread(file = file.path(dir_misc, stem, "academiesmatmembership20250626.csv"))

# fix col names
names(groups) <- tolower(names(groups))
names(groups) <- gsub("(", "", names(groups), fixed = T)
names(groups) <- gsub(")", "", names(groups), fixed = T)
names(groups) <- gsub(" ", "_", names(groups), fixed = T)

groups$establishmentname <- iconv(groups$establishmentname, from = "latin1", to = "UTF-8")
groups$group_name <- iconv(groups$group_name, from = "latin1", to = "UTF-8")

# filter rows
groups <- groups[group_status != "Closed"]
groups <- groups[establishmentstatus_name != "Closed"]
groups <- groups[group_type == "Multi-academy trust"]

# select cols
groups <- groups[, .SD, .SDcols = 
                grepl("urn|la_|number|id|date|phase|reason|establishmentname|group_name", names(groups))]
groups <- groups[, .SD, .SDcols = 
             !grepl("n_code|d_code|companies|ofsted", names(groups))]

# add laestab
groups[, laestab := as.numeric(gsub("/", "", dfe_number))]

# add region
lookup <- fields[, la_name, gor_name]
lookup <- lookup[!duplicated(lookup)]
lookup <- lookup[!gor_name %in% c("Not Applicable", "Wales (pseudo)")]

groups <- merge(lookup, groups, by = "la_name", all.y = T)
groups <- groups[!is.na(urn)] # remove MATs without any schools

# save file
fwrite(groups, file = file.path(dir_data, "data_establishments_groups.csv"), bom = T)