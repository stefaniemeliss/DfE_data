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

# The difference is Children's centre and Children's centre linked sites are not included in the all establishment data extract (DOWNLOAD) 
# but they will be included in the results (SEARCH) from the find an establishment search if they are not filtered out.

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

#### source: https://get-information-schools.service.gov.uk/Downloads ####
stem <- "extract_gias_download_20251125"
# date downloaded 25/11/2025
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

# Establishment fields
file_fields <- list.files(path = file.path(dir_misc, stem), pattern = "^edubasealldata", full.names = T)
fields <- fread(file = file_fields)

# fix col names
names(fields) <- tolower(names(fields))
names(fields) <- gsub("(", "", names(fields), fixed = T)
names(fields) <- gsub(")", "", names(fields), fixed = T)
names(fields) <- gsub(" ", "_", names(fields), fixed = T)

fields$establishmentname <- iconv(fields$establishmentname, from = "latin1", to = "UTF-8")

# replace all "" with NA
fields <- fields[, lapply(.SD, function(x) replace(x, which(x==""), NA))]

# Ensure laestab_number has leading zeros and concatenate with dfe_number
fields[, establishmentnumber_str := ifelse(!is.na(establishmentnumber), sprintf("%04d", establishmentnumber), NA)]
fields[, la_code_str := ifelse(!is.na(la_code), sprintf("%03d", la_code), NA)]
fields[, dfe_number := ifelse(!is.na(establishmentnumber) & !is.na(la_code), paste0(la_code_str, "/", establishmentnumber_str), NA)]
fields[, laestab := ifelse(!is.na(establishmentnumber) & la_code != 0, as.numeric(paste0(la_code_str, establishmentnumber_str)), NA)]
fields$establishmentnumber_str <- NULL
fields$la_code_str <- NULL

# format dates
fields[, opendate := as.Date(opendate, format = "%d-%m-%Y")]
fields[, closedate := as.Date(closedate, format = "%d-%m-%Y")]

# fix religion #
# Create a regex pattern to match Christian denominations
p <- paste0(c("Church", "Catholic", "Christian", "Anglican", "Methodist", "Adventist", "Evangelical", "Protestant", "Moravian", "Quaker", "Baptist"), collapse = "|")

# Identify Christian denominations
fields[, religiouscharacter_christian := grepl(pattern = p, religiouscharacter_name, ignore.case = TRUE)]

sum(is.na(fields$boarders_code))
sum(is.na(fields$boarders_name))
unique(fields$boarders_code)
unique(fields$boarders_name[fields$boarders_code == 9])

sum(is.na(fields$admissionspolicy_code))
sum(is.na(fields$admissionspolicy_name))
unique(fields$admissionspolicy_code)
unique(fields$admissionspolicy_name[fields$admissionspolicy_code == 9])

# rename columns
setnames(fields, "trusts_code", "group_uid")
setnames(fields, "la_code", "la_number")
names(fields)

# remove all columns containing a numeric code
dt <- fields[, .SD, .SDcols = 
               !grepl("_code", names(fields))]


# define col name pattern
p <- paste0(c(
  # school identifiers + info on estab type & status
  "urn", "la_", "estab", "dfe", 
  # open and close dates + reasons
  "open", "close", 
  # age of pupils            
  "phase", "statut", "nurs", "sixth", 
  # characteristics of school
  "boarders", "relig", "dioc", "admiss", "special", "senpru", 
  # characteristics of pupils
  "gender", "numberofpupils", "percentagefsm",
  # info on trusts, federations and sponsors
  # "trust", "group_uid", "sponsor", "federation",
  "flag", "group_uid", #"sponsor", "federation",
  # location and ONS admin info
  "urban", "street", "town", "postcode", "gor", "parlia", "district", "ward"), 
  collapse = "|")

# select cols using pattern
dt <- dt[, .SD, .SDcols = 
           grepl(pattern = p, names(dt))]

# remove cols not needed
dt <- dt[, .SD, .SDcols = 
            ! grepl("previous|boardingestablishment|accred", names(dt))]

# remove "_name" from column names
names(dt) <- gsub("_name", "", names(dt))

# rename columns
setnames(dt, "la_number", "la_code")

# select and re-order columns
out <- dt[, .(
  # school identifiers + info on estab type & status
  urn, laestab, dfe_number, la_code, establishmentnumber, establishmentname,
  # estab status
  establishmentstatus, opendate, reasonestablishmentopened, closedate, reasonestablishmentclosed,
  # estab type
  typeofestablishment, establishmenttypegroup,
  # age of pupils
  phaseofeducation, statutorylowage, statutoryhighage, nurseryprovision, officialsixthform,
  # characteristics of pupils
  gender, numberofpupils, percentagefsm,
  # characteristics of school
  senpru, specialclasses, boarders, religiouscharacter, religiouscharacter_christian, diocese, admissionspolicy,
  # admin
  trustschoolflag, group_uid, schoolsponsorflag, federationflag,
  # address
  urbanrural, street, postcode, town, gor, la, districtadministrative, administrativeward, parliamentaryconstituency)]

# sort
setorder(out, urn)

# Establishment links #
file_links <- list.files(path = file.path(dir_misc, stem), pattern = "links_edubasealldata", full.names = T)
links <- fread(file = file_links)
names(links) <- tolower(names(links))
links$linkname <- iconv(links$linkname, from = "latin1", to = "UTF-8")

# replace all "" with NA
links <- links[, lapply(.SD, function(x) replace(x, which(x==""), NA))]
# process date
links[, linkestablisheddate := as.Date(linkestablisheddate, format = "%d-%m-%Y")]

# select columns
links <- links[, c("urn", "linkurn", "linktype")]

# create counter
links[, counter := seq_len(.N), by = urn]
links[, counter := sprintf("%02d", counter)]

# reshape from long to wide
links <- reshape(links, idvar = "urn", timevar = "counter", direction = "wide", sep = "_")

# add link data
gias <- merge(out, links, by = "urn", all.x = T)

# save file
fwrite(out, file = file.path(dir_data, "data_gias_estab.csv"), bom = T)

# Establishment groups #

# read data
file_groups <- list.files(path = file.path(dir_misc, stem), pattern = "academiesmatmembership", full.names = T)
groups <- fread(file = file_groups)

# fix col names
names(groups) <- tolower(names(groups))
names(groups) <- gsub("(", "", names(groups), fixed = T)
names(groups) <- gsub(")", "", names(groups), fixed = T)
names(groups) <- gsub(" ", "_", names(groups), fixed = T)
setnames(groups, "la_code", "la_number")

groups$establishmentname <- iconv(groups$establishmentname, from = "latin1", to = "UTF-8")
groups$group_name <- iconv(groups$group_name, from = "latin1", to = "UTF-8")

# replace all "" with NA
groups <- groups[, lapply(.SD, function(x) replace(x, which(x==""), NA))]

# # filter rows
# groups <- groups[group_status != "Closed"]
# groups <- groups[establishmentstatus_name != "Closed"]
# groups <- groups[group_type == "Multi-academy trust"]

# select cols
groups <- groups[, .SD, .SDcols = 
                   grepl("urn|dfe|la_|estab|group", names(groups))]

groups <- groups[, .SD, .SDcols = 
                   ! grepl("_code|ukprn|companies|street|local|address|town|county|postcode", names(groups))]
setnames(groups, "la_number", "la_code")

# add laestab
groups[, laestab := as.numeric(gsub("/", "", dfe_number))]

# add region
lookup <- fields[, la_name, gor_name]
lookup <- lookup[!duplicated(lookup)]
lookup <- lookup[!gor_name %in% c("Not Applicable", "Wales (pseudo)")]

groups <- merge(lookup, groups, by = "la_name", all.y = T)
groups <- groups[!is.na(urn)] # remove MATs without any schools

# remove "_name" from column names
names(groups) <- gsub("_name", "", names(groups))

# save file
fwrite(groups, file = file.path(dir_data, "data_gias_groups.csv"), bom = T)
