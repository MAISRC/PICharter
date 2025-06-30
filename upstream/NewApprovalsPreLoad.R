#To be sourced by the NewSubmissionsApproval.R file once per approval process


# Load packages -----------------------------------------------------------

library(lubridate)
library(sf)
library(gargle)
library(googledrive)
library(googlesheets4)
library(data.table)
library(dplyr)
library(stringr)
library(stringi)
library(tools)
library(arrow)
library(shiny)

# Set global authentication options ------------------------------------------------------

#SET SCOPES FOR WHAT OUR GOOGLE APIs WILL BE ALLOWED TO DO
sheets_scope <- "https://www.googleapis.com/auth/spreadsheets"
drive_scope <- "https://www.googleapis.com/auth/drive"

#SET FILE PATH TO THE GOOGLE SERVICE ACCOUNT TOKEN
sa_key_path <- "PI_Survey/.secrets/picharter-57080685c8d0.json"

#DON'T TRY TO AUTHENTICATE USING STANDARD INTERACTIVE SESSION TOKENS.
options(
  gargle_oauth_cache = FALSE
)

#AUTHENTICATE USING OUR JSON TOKEN ATTACHED TO OUR GOOGLE SERVICE ACCOUNT W/ PROPER SCOPES
drive_auth(path = sa_key_path, scopes = c(sheets_scope, drive_scope))
googlesheets4::gs4_auth(path = sa_key_path, scopes = c(sheets_scope, drive_scope))

# Load pre-req files and functions ------------------------------------------------------

tidyName <- function(x) {
  base::tolower(
    stringr::str_replace_all(x,
                             pattern = "([\\.\\(\\)\\-\\/\\?])|([_]+)|([\\s]+)",
                             replacement = "_")
  )
}

#REWRITE TO GDRIVE ANON FUNCTIONS

# #RE-WRITE FILE TO GDRIVE--THESE STEPS REPEAT MANY TIMES, COMMENTED ONLY HERE.
# tmp.path = base::file.path(base::paste0(base::tempdir(), "\\")) #MAKE TEMP PATH
# file.tmp = paste0(tmp.path, metadata_row$CLEAN_FILE) #MAKE FILE NAME
# write.csv(x = current.import, file = file.tmp, row.names = FALSE) #WRITE LOCAL OBJ
# googledrive::drive_upload(media = file.tmp, #SHIP TO GDRIVE
#                           path = submitted_clean_id,
#                           name = metadata_row$CLEAN_FILE) #RESULTS IN MULTIPLE VERSIONS OF THE SAME FILE (CAN'T GIVE OVERWRITE PRIVLEDGES), SO THESE MUST BE MANUALLY TIDIED.

rewritetoG = function() {
  tmp.path = base::file.path(base::paste0(base::tempdir(), "\\")) 
  file.tmp = paste0(tmp.path, metadata_row$CLEAN_FILE) 
  write.csv(x = current.import, file = file.tmp, row.names = FALSE) 
  googledrive::drive_upload(media = file.tmp, 
                            path = submitted_clean_id,
                            name = metadata_row$CLEAN_FILE)
}

#SMART COL CONVERSION, HERE UNIQUE FROM GLOBAL.R IN THAT IT CONVERTS FIRST TO NUMERIC, OTHERWISE FACTOR FOR NICE SUMMARIES.
convert_column_types_approv <- function(df) {
  suppressWarnings(lapply(df, function(column) {

    #ATTEMPT TO CONVERT TO NUMERIC NEXT
    numeric_column = column
    numeric_column[is.na(numeric_column)] = 0
    numeric_column <- as.numeric(numeric_column)
    if (!anyNA(numeric_column)) {
      return(numeric_column)
    }
    
    #OTHERWISE DEFAULT TO CHARACTER
    return(as.factor(column))
  }))
}

#THE PLAIN CONVERT COLUMN TYPES FUNCT, WHICH STILL REDUCES TO CHARACTERS WHERE NECESSARY
convert_column_types <- function(df) {
  suppressWarnings(lapply(df, function(column) {
    # #ATTEMPT TO CONVERT TO LOGICAL IF NO NAS
    # logical_column <- as.logical(column)
    # if (!anyNA(logical_column)) {
    #   return(logical_column)
    # }
    #ATTEMPT TO CONVERT TO NUMERIC NEXT
    numeric_column <- as.numeric(column)
    if (!anyNA(numeric_column)) {
      return(numeric_column)
    }
    
    #OTHERWISE DEFAULT TO CHARACTER
    return(as.character(column))
  }))
}

#LOAD IN LOOKUP TABLE FOR DETERMINING WHICH COLS ARE RECOGNIZED, WHETHER THEY ARE TAXONOMIC.
newfieldnames = utils::read.csv("inputs/Dynamic/column_name_lookup.csv") %>% 
  dplyr::mutate(newfieldname = tidyName(newfieldname)) %>%
  dplyr::select(newfieldname, taxonomic) %>%
  dplyr::distinct()

#LOAD ADDITIONAL FILES WHEREIN ANY NEW COL NAMES WOULD NEED TO BE ADDED (FOR A CHECK)
directoryfile = utils::read.csv("inputs/Dynamic/PICharterFieldNames.csv")
nametogglefile = utils::read.csv("inputs/Static/commonsci_name_lookup.csv")

#GET ACCESS TO THE METADATA FILE FOR ITS FLAGS AND METADATA
metadata_id = googledrive::drive_get("https://docs.google.com/spreadsheets/d/1iqJThuAjoMhRs1njvWPi_7pbszM-Dlvl933L6wsPR6k/edit?usp=sharing")$id
metadata_sheet = googlesheets4::read_sheet(ss = metadata_id,
                                           sheet = "submitted_metadata")

#Load in a data set for validating DOW numbers.
all_DOWs = utils::read.csv("inputs/Static/dows_lakenames_counties.csv", colClasses = "character") %>% 
  dplyr::select(DOW) %>% 
  dplyr::pull()

# Pre QA/QC stops, checksums, prompts, etc. -------------------------------------

#ARCHIVE PREV DB FILE BEFORE CONTINUING
archival_path = "H:\\Shared drives\\MAISRC\\Quantification, Data, and Computation\\Projects\\Statewide Plant Surveys App\\PI Charter\\upstream\\Archived database summaries" #ARCHIVE FOLDER
archival_filename = paste0("\\picharter_dbnew", Sys.Date(), ".parquet") #NEW FILE NAME
current_dbname = "H:\\Shared drives\\MAISRC\\Quantification, Data, and Computation\\Projects\\Statewide Plant Surveys App\\PI Charter\\upstream\\db_unified.parquet" #CURRENT FILE'S NAME
current_db = read_parquet(current_dbname) #READ
write_parquet(as.data.frame(current_db), sink = paste0(archival_path, archival_filename)) #WRITE

# Access the GDrive's "clean" folder and discover contents ---------------

submitted_raw_id = googledrive::drive_get("https://drive.google.com/drive/folders/1IRSvqDu5idxQBfJqYXlxDCl9Yo8WlN2l")$id
submitted_clean_id = googledrive::drive_get("https://drive.google.com/drive/folders/1LwxRFKd9m8fNPFB0Fgl4n9v9oBRP4_FF")$id
already_approved_id = googledrive::drive_get("https://drive.google.com/drive/folders/17YNVoTJ6cTVjaTzuDNLy48DsL1VhSQ_L")$id 
need_fixes_id = googledrive::drive_get("https://drive.google.com/drive/folders/1y7x9EBNgmG6YqUlKc6lb9LZKxSKaBLKb")$id 
already_approved_name = googledrive::drive_get("https://drive.google.com/drive/folders/17YNVoTJ6cTVjaTzuDNLy48DsL1VhSQ_L")$name 
need_fixes_name = googledrive::drive_get("https://drive.google.com/drive/folders/1y7x9EBNgmG6YqUlKc6lb9LZKxSKaBLKb")$name
