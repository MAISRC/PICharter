# Load libraries ----------------------------------------------------------

library(shiny)
library(dplyr) #Used for df manipulation throughout. 
library(stringr) #Various string-based operations throughout. 
library(gargle) #Interfacing with google drive. 
library(googledrive) #Enables beaming files to/from Google Drive
library(googlesheets4) #Enables writing into a Google Sheets file.
library(shinyjs) #For disabling/enabling buttons as well as hiding inputs
library(DT) #For spiffy Data Tables
library(waiter) #Used a couple of times to add some busy spinners, but deemed essential.
library(readxl) #Used only in 1-2 places to read in Excel files, but pretty essential.
library(sf) # For doing work with Mike's geometry data.
library(leaflet) #Get Leaflet maps going.
library(shinyvalidate) #For cleanly validating email addresses. Could probably be eliminated later when the Submissions tab is reborn. 
library(shinydisconnect) #For customizing the terrible disconnect screen.
library(arrow) #For working with large data files. 
library(data.table) #Might be needed for downloading the parquet files? Unclear...
library(plotly) #For interactive graphs
library(tidyr) #For a pivot_longer
library(viridis) #For a colorblind graph palette
library(shinyWidgets) #FOR PICKERINPUTS, WHICH ARE MUCH IMPROVED SELECTINPUTS FOR MULTI-SELECTION

# Set global options ------------------------------------------------------

#SET SCOPES FOR WHAT OUR GOOGLE APIS WILL BE ALLOWED TO DO
sheets_scope <- "https://www.googleapis.com/auth/spreadsheets"
drive_scope <- "https://www.googleapis.com/auth/drive"

#SET FILE PATH TO THE GOOGLE SERVICE ACCOUNT TOKEN
sa_key_path <- "PI_Survey/.secrets/picharter-57080685c8d0.json"

#SET NO LARGE FILES AND ALSO DON'T TRY TO AUTHENTICATE USING STANDARD TOKENS.
options(
  shiny.maxRequestSize = 20 * 1024 ^ 2,
  gargle_oauth_cache = FALSE
  )

#AUTHENTICATE USING OUR JSON TOKEN ATTACHED TO OUR GOOGLE SERVICE ACCOUNT W/ PROPER SCOPES
drive_auth(path = sa_key_path, scopes = c(sheets_scope, drive_scope))

#ALSO AUTHENTICATE SUCH THAT GOOGLE SHEETS IS HAPPY
googlesheets4::gs4_auth(path = sa_key_path, scopes = c(sheets_scope, drive_scope))

# Set up page-generating functions from the Rcode/pages subfolder ----------------------------------------
source("Rcode/pages/LeaderboardUI.R")
source("Rcode/pages/LeaderboardServer.R")
source("Rcode/pages/BrowseUI.R")
source("Rcode/pages/BrowseServer.R")
source("Rcode/pages/SubmissionsUI_V2.R")
source("Rcode/pages/SubmissionsServer_V2.R")
source("Rcode/pages/RecordsUI.R")
source("Rcode/pages/RecordsServer.R")

#SUBMISSIONS GENERIC INPUT VALIDATION FUNCTION --------------------------------------------------

subValidate = function(currTab, inputValue) {
  
  #BASICALLY, CHECK WHICH TAB WE'RE ON, DO SOME TEXT ON ITS CURRENT VALUE, RETURN TEST RESULT
  if(currTab == 1) { 
    return(
      inputValue == "Yes, I understand; my current submission will not include any non-throws." |
      inputValue == "Yes, I understand; my current submission will include non-throws, and I will need the app to help me eliminate them." #RIGHT ANSWERS
    )
  }
  
  if(currTab == 2) {
    return(
      isTruthy(inputValue) & 
        nchar(as.character(inputValue)) >= 8 & #AT LEAST 8 CHARACTERS, NO DIGITS
        !grepl("\\d", as.character(inputValue))
    )
  }
  
  if(currTab == 3) {
    return(
      isValidEmail(inputValue) #MUST BE EMAIL
      )
  }
  
  if(currTab == 4) {
    return(
      isTruthy(inputValue) & 
      (nchar(as.character(inputValue)) >= 8 | #AT LEAST 8 CHARACTERS OR CONTAIN 'NA' SUBSTRING
        grepl("NA", as.character(inputValue), ignore.case = T))
    )
  }
  
  if(currTab == 5) {
    return(
      isTruthy(inputValue) &&
        inputValue != "Select a lake" #WRONG ANSWER
    )
  }
  
  if(currTab == 6) {

    return(
        isTruthy(inputValue) && 
          grepl("\\D", base::substr(inputValue,1,5)) && #FIRST FIVE VALS CAN'T ALL BE DIGITS, MUST BE 10 CHARACTERS, MUST NOT CONTAIN 2 DIGIT YEARS, MUST BE BEFORE TODAY'S DATE, MUST BE PAST YEAR 1980.
        nchar(as.character(inputValue)) == 10 &&
        !grepl("-00", as.character(inputValue), fixed = TRUE) &&
        !grepl("/00", as.character(inputValue), fixed = TRUE) &&
        as.Date(inputValue) < Sys.Date() &&
        as.numeric(str_split(as.character(inputValue), "-")[[1]][1]) >= 1980
        )
  }
  
  if(currTab == 7) {
    return(
      inputValue != "No selection" #WRONG ANSWER
    )
  }
  
  if(currTab == 8) {
    return(
      inputValue != "No selection" #WRONG ANSWER
    )
  }
  
  if(currTab == 9) {
    
    if(isTruthy(inputValue$datapath)) { 
    
    return(
        (grepl(".csv", inputValue$datapath, ignore.case = T) | #MUST HAVE VALID FILE EXTENSION, AND IF EXCEL FILE, JUST 1 SHEET
        grepl(".tsv", inputValue$datapath, ignore.case = T) |
        grepl(".zip", inputValue$datapath, ignore.case = T)) ||
        (grepl(".xls", inputValue$datapath, ignore.case = T) &&
        length(readxl::excel_sheets(inputValue$datapath)) == 1
    ))
      
    } else { #***WHY THE ELSE FALSE NEEDED HERE?
      return(FALSE)
    }
    
  }
  
  if(currTab == 10) {
    return(
      isTruthy(inputValue) &
        inputValue > 0 #MUST BE >0. ADDITIONAL DYNAMIC VALIDATION DONE ELSEWHERE.
    )
  }
  
  if(currTab == 11) {
    return(
    isTruthy(inputValue) #ADDITIONAL DYNAMIC VALIDATION DONE ELSEWHERE.
    )
  }
  
  if(currTab == 12) {
    return(
      inputValue != "No selection" #WRONG ANSWER 
   
       )
  }
  
  if(currTab == 13) {
    return(
      inputValue != "No selection" #WRONG ANSWER
    )
  }
  
  if(currTab == 14) {
    return(
      isTruthy(inputValue) #ANYTHING
    )
  }
  
  if(currTab == 15) {
    return(
      inputValue != "No selection" #WRONG ANSWER 
    )
  }
  
  if(currTab == 16) {
    return(
      isTruthy(inputValue) #ANYTHING
    )
  }
  
  if(currTab == 17) {
    return(
      inputValue != "No selection" #WRONG ANSWER 
    )
  }
  
  if(currTab == 18) {
    if(isTruthy(inputValue$datapath)) {
      
      return( #MUST BE A VALID FILE EXTENSION
        grepl(".csv", inputValue$datapath, ignore.case = T) |
          grepl(".tsv", inputValue$datapath, ignore.case = T) |
          grepl(".xls", inputValue$datapath, ignore.case = T) | 
          grepl(".gpx", inputValue$datapath, ignore.case = T) |
          grepl(".zip", inputValue$datapath, ignore.case = T) 
      )
    } else {
      return(FALSE)
    }
    
  }
  
  if(currTab == 19) {
    return(
      TRUE #ANYTHING
    )
  }
  
}

# Set up convenience functions --------------------------------------------

#FUNCTION FOR CHECKING IF A VALUE IS EVEN (FOR COLLAPSIBLES MOSTLY)
checkIfEven = function(val) {
  if(val %% 2 == 0) { 
    return(TRUE) 
  } else { return(FALSE) }
}

#FUNCTION TO OPEN/CLOSE MODALS DEPENDING ON BUTTON CLICKS
openCloseInfo = function(result, answer, question, session) {
  if(result) {
    shinyjs::hide(answer, anim=T, animType = "slide")
    updateActionButton(session, question, icon=icon("plus"))
    runjs(paste0("$('#", question, "').attr('aria-expanded', 'false');")) #These commands update the aria-expanded attribute to reflect whether the accordion is open or closed.
  } else {
    shinyjs::show(answer, anim=T, animType="slide")
    updateActionButton(session, question, icon=icon("minus"))
    runjs(paste0("$('#", question, "').attr('aria-expanded', 'true');"))
  }
}

#FUNCTION FOR DETECTING A VALID EMAIL ADDRESS
isValidEmail <- function(x) {
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>",
        as.character(x), 
        ignore.case=TRUE)
}

#Establish the tidyName function to get punctuation marks, capitals, spaces, etc. out of column names.
tidyName <- function(x) {
  base::tolower(
    # https://regexr.com/ for regular expressions
    stringr::str_replace_all(x,
                             pattern = "([\\.\\(\\)\\-\\/\\?])|([_]+)|([\\s]+)",
                             replacement = "_")
  )
}

#Establish a function for checking for date columns.
is.POSIXct <- function(x) {
  base::inherits(x, "POSIXct")
}

#Function to convert new taxonomic data to the same format as Mike's by capitalizing just the first letter in each entry. -->https://stackoverflow.com/questions/18509527/first-letter-to-upper-case
firstup <- function(x) {
  base::substr(x, 1, 1) <- base::toupper(base::substr(x, 1, 1))
  return(x)
}

#Function for splitting a string along commas, hunting for duplicates, keeping only those, then turning that list of values back into a string.
split.str_dupe_hunts = function(x) {
  tmp1 = stringr::str_split_1(x, ",")
  tmp2 = base::duplicated(tmp1) #I believe it's the base version that should be used here, not the one from data.table, so that's a potential conflict point.
  tmp3 = tmp1[tmp2]
  tmp4 = base::toString(tmp3)
  return(tmp4)
}

#Function for finding empty rows in a data frame and removing them. See function below, which uses this.
remove.emptyrows = function(df) {
  return(df[base::rowSums(base::is.na(df)) != base::ncol(df),])
}

#Function that removes values -999 and -9999, which some people use as 0s or blanks. See function below, which uses this.
remove.neg999s = function(df) {
  df[df == -999] = NA
  df[df == -9999] = NA
  return(df)
}

#Function that appends to the far-left of a data frame a column of row numbers.
add.rownumscol = function(df) {
  ROW_NUMBER.df = base::data.frame(ROW_NUMBER = 1:(base::nrow(df)))
  return(base::cbind(ROW_NUMBER.df, df))
}

#Function that bundles the processes for file in-loading.
initial.dfprocessing = function(df) {
  df = remove.emptyrows(df)
  df = add.rownumscol(df)
  base::row.names(df) = NULL #Can turn them off here just the once instead of in every downstream operation.
  df = remove.neg999s(df)
  return(df)
}

#Function that uses apply to check the column names from the user's submission against our lookup table and either find a set of replacement names corresponding to those in our database or else mark them as "delete" or "ambiguous" for flagging/removing.
colname.replacement = function(col.name) {
  #Prior to checking against the newfieldname column, we need to "tidy" it because it won't have been tidied yet. We can then check against both the tidied and untidied versions, just in case. However, we do this tidying in a temp object because we don't want the new column names "tidily-formatted" prior to submission because that would make them harder to read for users.
  newfieldnames_tidied = tidyName(newfieldnames$newfieldname)
  
  #We check to see if the col name supplied is in either of our columns in our lookup table. If it isn't, we mark it for deletion.
  name.check = base::which(
    newfieldnames$fieldname == col.name |
      newfieldnames$newfieldname == col.name |
      newfieldnames_tidied == col.name
  )
  
  if (isTruthy(name.check)) {
    newcolname = newfieldnames$newfieldname[name.check[1]] #So far as I can tell, there are never instances where name.check should pull back records from newfieldnames$newfieldname here that are not just duplicates of the same value, so the 1 here to pull the first one *should* be safe. We'd need an instance, I think, where a single entry would pull two records with differing newfieldnames, and I don't know of any such entries.
  } else {
    newcolname = "delete"
  }
}

#Function that uses user inputs to find and remove any records in a data frame that represent "non-throws" which are a type of record we aren't keeping in our database.
find.remove.nonthrows = function(df, non_throws, non_throws_val) {
  #An interesting case--if the user says that NA values mark non-throws, the selector will pass these in as "NA"s. So we check for and accommodate that here by appending a real NA whenever "NA" is a selected value for non_throws_val
  if ("NA" %in% non_throws_val) {
    inputs2throwout = base::c(base::as.character(non_throws_val), NA)
  } else {
    inputs2throwout = base::as.character(non_throws_val)
  }
  
  #Same handling for our new "Blanks" option in the non-throws selector, which will similarly pass in NA into the vector of values to filter by. If a user chooses both NA and Blanks in the selector, that shouldn't matter.
  if ("Blanks" %in% non_throws_val) {
    inputs2throwout = base::c(base::as.character(non_throws_val), NA)
  } else {
    inputs2throwout = base::as.character(non_throws_val)
  }
  
  #We just need to convert the column name string that comes into the function into a name object we can curly-curly inside of filter. See here --> https://stackoverflow.com/questions/61348475/how-to-use-tidy-evaluation-with-column-name-as-strings
  col.name.tmp = base::as.name(non_throws)
  
  #Now, trash anything in the chosen column that matches any value in the inputs2throwout string. This matching should all work because all entries should be characters by now, so we're always matching classes.
  current_df2 = df %>%
    dplyr::filter(!{{
        col.name.tmp
      }} %in% inputs2throwout)
  
  return(current_df2)
}

#Function that does the process of preparing a submitted (or manufactured) data file object for upload to Gdrive.
prepare_GDrive_upload = function(filepath) {
  ext = tools::file_ext(filepath) #Get the extension from the file submitted.
  
  tmp.path = base::file.path(base::paste0(base::tempdir(), "\\")) #Create a temp file location to move this file to as a staging ground.
  
  name.me = base::paste0(base::paste0(base::sample(c(LETTERS, 0:9), 20), collapse =
                                ""), ".", ext, sep = "") #Create randomized file name of 20 random letters and numbers.
  
  #Move the file from its initial temp location upon upload to its second temp location + rename it using the pre-generated name.
  base::file.copy(from = filepath,
                  to = base::paste0(stringr::str_remove(
                    tmp.path,
                    base::paste0(".", ext, sep =
                                   "")
                  ),
                  name.me))
  
  return(base::list(name.me = name.me, tmp.path = tmp.path))
  
}

#Function that does the processing necessary to turn a taxalist column into the appropriate options for a selectInput() for filtering by taxa. Needed to allow reactivity in this selector as counties are selected. This code is all annotated in the upstream 01 file so that is not repeated here.
process_taxalist = function(taxalist) {
  selector_taxa = base::unique(base::trimws(stringr::str_split_1(
    base::paste(taxalist, collapse = ","), ","
  )))
  
  selector_taxa = selector_taxa[!base::grepl("Protected Species", selector_taxa)]
  
  db_allSearchableTaxa = base::sort(selector_taxa)
  
  db_allSearchableTaxa =
    db_allSearchableTaxa[!base::grepl(" taxa", db_allSearchableTaxa, ignore.case = T)]
  
  db_allSearchableTaxa = base::gsub("spp.", "(any)", db_allSearchableTaxa, ignore.case = T)
  return(db_allSearchableTaxa)
}

#Function that does the processing necessary to turn a list of dows and lake names into the appropriate options for the selectInput() for filtering by lakes. Needed to allow reactivity in this selector as counties are selected. This code is all annotated in the upstream 01 file so that annotation is not repeated here.
process_dowslakes = function(dows, names) {
  d3 = base::paste0(dows, " (", tools::toTitleCase(names), ")")
  db_allDOWs = base::sort(base::unique(d3))
  return(db_allDOWs)
}


#Function converts a lookup table into a function that does find and replacement in a vectorized way. It's fine, speed-wise, if the lookup space is small but lousy at large sizes.
replace_names <- function(strings, toreplace, replacements) {

  replacement.vec <- setNames(replacements, toreplace) #Make a named vector with the replacements as the names.

  stringr::str_replace_all(strings, replacement.vec) #Use this function to do the string replacements.
}

#Function that smartly transposes a data frame, preserving the original data types of the former rows.
smart_transpose <- function(df) {
  
  df2 = df[,-1] #Nix first column, as that will be the new column names eventually. 
  
  # Convert each column in the data set to a list.
  transposed_list <- base::lapply(base::seq_along(df2), function(i) { df2[[i]] } )
  
  # Convert the lists to a data frame using rbind and re-establish column names.
  transposed_df <- stats::setNames(base::data.frame(base::do.call(rbind, transposed_list), 
                                             stringsAsFactors = FALSE), 
                                  df[, 1])
  
  # Reset row names
  base::row.names(transposed_df) <- NULL
  
  return(transposed_df)
}

#DO SMART TYPE INFERENCE ON THE COLUMNS OF A DF
convert_column_types <- function(df) {
  suppressWarnings(lapply(df, function(column) {

    #ATTEMPT TO CONVERT TO NUMERIC NEXT
    numeric_column <- as.numeric(column)
    if (!anyNA(numeric_column)) {
      return(numeric_column)
    }

    #OTHERWISE DEFAULT TO CHARACTER
    return(as.character(column))
  }))
}

#FOR PLOTLY GRAPH LABELS, REDUCE SPP NAMES TO 4 CHAR ABBRS.
abbreviate_name <- function(name) {
  words <- str_split(name, " ")[[1]]  #SPLIT NAME INTO WORDS

  abbrev_words <- sapply(words, 
                         function(word) { #FOR EACH WORD

                           #GET FIRST 4 CHARS (OR LESS).
                           word.tmp = substr(word, 1, min(4, nchar(word))) 
                           
                           #IF THE ORIGINAL WORD WAS LONGER THAN 4 CHARACTERS, GIVE IT A PERIOD TO INDICATE IT WAS ABBREVED.
                           if(nchar(word) > 4) {
                             word.tmp = paste0(word.tmp, ".")
                           }
                           word.tmp
                           }
                         )
  
  abbr = paste(abbrev_words, collapse = " ") #COMBINE EACH NEW ABBR INTO 1 STRING

  return(abbr)
}

#VECTORIZED VERSION OF THE ABOVE.
abbreviate_names = function(names) {
  unname(sapply(names, abbreviate_name))
}

# Load all necessary inputs -----------------------------------------------

#All "Made Upstream" inputs.
surveyors.leaderboard.df = base::readRDS("inputs/MadeUpstream/surveyors_leaderboard_df") #Small table containing every surveyor on at least one survey in the database along with how many surveys they've been on.
lakes.summary.definitive = base::readRDS("inputs/MadeUpstream/lakes_summary_definitive") #The definitive edition of the database's summary data, aggregated at the lake level.
lakes.summary.sf = base::readRDS("inputs/MadeUpstream/lakes_summary_sf") #Same as above, but with simple features geometry attached for mapping. Having both prevents having to "detach" the sf geometry whenever it's not needed.
db_allDOWs = base::readRDS("inputs/MadeUpstream/db_allDOWs") #From the database, a list of every lake that we have at least one survey from (by DOW number). For an input selector.
db_allCounties = base::readRDS("inputs/MadeUpstream/db_allCounties") #From the database, a list of every county we have at least one surveyed lake for. For an input selector.
db_allSearchableTaxa = base::readRDS("inputs/MadeUpstream/db_allSearchableTaxa") #From the database, a list of all taxa observed, albeit aggregated for use in a selector so that, for example, we can select "any" taxon from a given genus rather than observations like "genus sp.".
db_allSearchableTaxa = db_allSearchableTaxa[!db_allSearchableTaxa %in% c("Certatyphylum echinatum")] #Getting rid of some typos, but this probably shouldn't be occurring here. 
submitter_leaderboard = base::readRDS("inputs/MadeUpstream/submitter_leaderboard") #The submitter's leaderboard data
protected_species = read.csv("inputs/Static/protected_nameskey.csv") #A list of protected species we must scrub from records.
protected_species$taxon_tidy = tidyName(protected_species$TAXON) #Tidy the scientific names for column name matching.

#Load in a data set for validating DOW numbers and reporting which lake is associated with a DOW given.
dows_lakenames_counties = utils::read.csv("inputs/Static/dows_lakenames_counties.csv", colClasses = "character")

#Import a constantly curated "glossary" of column field names we've seen across submissions so far. That way, we can recognize whether we want to keep a column or not and also which columns contain the same data and thus should be vertically bound together as data sets are joined to the database. Make sure to always save this file as a UTM-8 file or gsub won't recognize the characters correctly!
newfieldnames = utils::read.csv("inputs/Dynamic/column_name_lookup.csv") %>% 
  dplyr::mutate(fieldname = tidyName(fieldname)) %>%  #For streamlined operations, we "pre-process" all the field names in our lookup guide. We will do the same to each submission's field names also so that formatting, spaces, etc. don't cause column names we're familiar with to go unrecognized.
  dplyr::select(fieldname, newfieldname, taxonomic) %>% #Remove unneeded columns from the guide and also remove any duplicates
  dplyr::distinct()

#Load a file that is a lookup table to convert between scientific and common names.
commonsciNameLookup = read.csv("inputs/Static/commonsci_name_lookup.csv")
commonsciNameLookup$sciname = firstup(gsub("_", " ", commonsciNameLookup$newfieldname)) #Create a new, untidied version for proper matching.
commonsciNameLookup$sciname = gsub(" sp$", " spp.", commonsciNameLookup$sciname) #For proper matching in the table for this substring.
# commonsciNameLookup$sciname = gsub(" spp.i", " spi", commonsciNameLookup$sciname) #Unfortunate edge case.
# commonsciNameLookup$sciname = gsub(" spp.on", " spon",  commonsciNameLookup$sciname) #Unfortunate edge case.
commonsciNameLookup$sciname_selector = gsub(" spp.", " \\(any\\)", commonsciNameLookup$sciname, fixed=T) #For matching with items in the selector. 

#Load in a list of the names of declared "official" invasive species. For marking invasives in the browse tab.
invasives_list = read.csv("inputs/Dynamic/invasives_list.csv")
##The following only needs to be run when updates to the invasive species list occur.
# invasives_list$scientific = gsub("×", "x", invasives_list$scientific) #Replace a times sign that snuck in.
# invasives_list$common = invasives_list$scientific
# invasives_list$common = replace_names(invasives_list$common,  #Find and replace scientific names with common ones so common names can be toggleable and still highlighted similarly in the table.
#                                       commonsciNameLookup$sciname, 
#                                       commonsciNameLookup$CommonName)

#LIST OF DOWS ON TRIBAL LANDS FOR OBSCURING
tribal_DOWs = readRDS("inputs/Static/tribal_DOWs.rds")
ourlakes = lakes.summary.definitive$DOW #COMPARE OUR LAKES TO THE TRIBES'
tribal_conflicts = ourlakes[which(ourlakes %in% tribal_DOWs)] #FIND THOSE WE HAVE RECORDS OF. THESE WILL NEED TO BE OBSCURED.

MN = base::readRDS("inputs/Static/MN") #Outline of Minnesota's polygon for mapping on the browse tab.

#Get the sheet ids for the metadata GDrive sheet and google folders for file submissions.
 metadata_id = googledrive::drive_get("https://docs.google.com/spreadsheets/d/1iqJThuAjoMhRs1njvWPi_7pbszM-Dlvl933L6wsPR6k/edit?usp=sharing")$id
 rawdata_id = googledrive::drive_get("https://drive.google.com/drive/folders/1IRSvqDu5idxQBfJqYXlxDCl9Yo8WlN2l")$id
 cleandata_id = googledrive::drive_get("https://drive.google.com/drive/folders/1LwxRFKd9m8fNPFB0Fgl4n9v9oBRP4_FF")$id
 locdata_id = googledrive::drive_get("https://drive.google.com/drive/folders/14ASYFti6Yfjy3Bv2K7lpwQn5t9J46Y7P")$id

db_path = "inputs/MadeUpstream/db_unified.parquet"

#GENERATE A LIST OF LAKE DOWS THAT ARE ONLY THOSE THAT HAVE AT LEAST SOME SPATIAL DATA.
spatial_dows = db_path %>% 
  open_dataset() %>% 
  filter(!is.na(latitude) & !is.na(longitude)) %>% #MUST HAVE BOTH LAT AND LONG DATA FOR AT LEAST ONE PT.
  select(DOW) %>%
  distinct() %>% 
  collect() %>% 
  pull()

#GET TAXONOMIC COL NAMES
taxonomic = newfieldnames %>% 
  filter(taxonomic == "Y") %>% 
  select(newfieldname) %>% 
  distinct() %>%
  pull()

#LOAD DIRECTORY OF FIELDNAMES FOR RECORDS TAB FILES
fieldNameDirectory = read.csv("inputs/Dynamic/PICharterFieldNames.csv")
 
# Establish shortcut objects -----------------------------------------------------

rows_per_page <- 10 #DEFAULT NUMBER OF RESULTS PER DT PAGE.

#ON THE RECORDS TAB, WE'LL PREPOPULATE THE DOW SELECTOR WITH DOWS PULLED FROM LAKES.SUMMARY.DF SINCE THESE SHOULD BE UP TO DATE. THIS IS A SOUPED-UP VERSION BY CHATGPT THAT IS WAY QUICKER THAN USING MY REPLACE_NAMES FUNCTION AT THIS SCALE. IT RELIES ON A JOIN TO FIND MATCHES WHICH IS MUCH FASTER THAN A ROW-BY-ROW REGEX. 
dows_list = tibble(dow = sort(unique(lakes.summary.definitive$DOW))) #MAKE A TIBBLE OF DOWS
dows_list = dows_list[!dows_list$dow %in% tribal_conflicts,] #REMOVE TRIBAL LAKES

#JOIN THE NAMES IN FROM ELSEWHERE
dows_list = dows_list %>% 
  left_join(dows_lakenames_counties[, c("DOW", "NAME")], by = c("dow" = "DOW")) %>%   mutate(name = if_else(is.na(NAME), as.character(dow), NAME)) %>% #JUST MAKES SURE WE KEEP SOMETHING IF THERE ISN'T A MATCH
  mutate(dow_name = paste0(dow, " (", name, ")")) #COMBINES IN THE STEP OF MAKING THE "PRETTY" SELECTOR LABOR.

#EXTRACT FOR USE IN THE SELECTOR. 
dowslist2use = dows_list$dow
names(dowslist2use) = dows_list$dow_name

#In the browse tab, I need to make hover labels for every lake. I was doing that "manually" each time in each of the relevant observers. However, it would be easier to do that just once here and append the resulting list as a column to the lakes.summary.sf object from the outset. This is a souped-up version from ChatGPT that just uses vectorized pasting based on the non-sf version that is lightning quick compared to what was here. 
lakes.summary.sf$maplabels = as.list(paste0(
  "Lake: ", 
  stringr::str_to_title(lakes.summary.definitive$LAKE_NAME), 
  "<br>",
  "DOW: ", 
  lakes.summary.definitive$DOW, 
  "<br>",
  "County: ", 
  lakes.summary.definitive$cty_name
))

#GRAB LAKE NAMES AND DOWS TO POTENTIALLY USE FOR THE LAKE NAME SELECTOR
names2display = paste0(
  dows_lakenames_counties$NAME, 
  " (",
  as.character(dows_lakenames_counties$DOW),
  ")"
)

lake.dows = dows_lakenames_counties$DOW
names(lake.dows) = names2display
lakedownames = names(lake.dows)


#SLIDE AND FADE ANIMATIONS FOR THE SUBMISSIONS TAB PANELS
numQuestions = 19

# START FADE-OUT ANIMATION ON CURRENT TAB, SLIDE AND FADE IN ANIMATION ON NEXT TAB (NEXT BUTTON)
slideIn = function(currtab, nexttab, numQuestions) {
  
  #FIRST, REMOVE ALL RELATED CLASSES FROM ALL TAB PANES
  for (i in 1:numQuestions) {
    shinyjs::removeClass(selector = paste0(
      "div.submissionTab>main>form>div.tabbable>div.tab-content>div.tab-pane:nth-child(", i, ")"), 
      class = "fade-in")
    shinyjs::removeClass(selector = paste0(
      "div.submissionTab>main>form>div.tabbable>div.tab-content>div.tab-pane:nth-child(", i, ")"), 
      class = "fade-inL")
    shinyjs::removeClass(selector = paste0(
      "div.submissionTab>main>form>div.tabbable>div.tab-content>div.tab-pane:nth-child(", i, ")"), 
      class = "fade-out")
  }
  
  #ADD FADE-OUT CLASS TO CURRENT TAB
  shinyjs::addClass(selector = paste0(
    "div.submissionTab>main>form>div.tabbable>div.tab-content>div.tab-pane:nth-child(", currtab, ")"), 
                    class = "fade-out")
  Sys.sleep(0.5) #ALLOW SOME TIME FOR THIS TO PLAY OUT.
  
  #ADD SLIDE IN CLASS TO NEXT TAB
  shinyjs::addClass(selector = paste0(
  "div.submissionTab>main>form>div.tabbable>div.tab-content>div.tab-pane:nth-child(", nexttab, ")"), 
  class = "fade-in")
  
  # ADD EVENT LISTENER TO NEXT TAB TO SCROLL INTO VIEW WHEN ANIMATION ENDS
  js_code <- sprintf("
    var nextTab = document.querySelector('div.submissionTab > main > form > div.tabbable > div.tab-content > div.tab-pane:nth-child(%d)');
    if (nextTab) {
      nextTab.addEventListener('animationend', function() {
        nextTab.scrollIntoView({ behavior: 'smooth', block: 'start' });
      }, { once: true });
    }
  ", nexttab)
  
  shinyjs::runjs(js_code)

}

# START FADE-OUT ANIMATION ON CURRENT TAB, SLIDE AND FADE IN ANIMATION ON NEXT TAB (BACK BUTTON). LARGELY SAME AS ABOVE
slideInL = function(currtab, nexttab, numQuestions) {
  
  for (i in 1:numQuestions) {
    shinyjs::removeClass(selector = paste0(
      "div.submissionTab>main>form>div.tabbable>div.tab-content>div.tab-pane:nth-child(", i, ")"), 
      class = "fade-in")
    shinyjs::removeClass(selector = paste0(
      "div.submissionTab>main>form>div.tabbable>div.tab-content>div.tab-pane:nth-child(", i, ")"), 
      class = "fade-inL")
    shinyjs::removeClass(selector = paste0(
      "div.submissionTab>main>form>div.tabbable>div.tab-content>div.tab-pane:nth-child(", i, ")"), 
      class = "fade-out")
  }
  
  shinyjs::addClass(selector = paste0(
    "div.submissionTab>main>form>div.tabbable>div.tab-content>div.tab-pane:nth-child(", currtab, ")"), 
    class = "fade-out")
  Sys.sleep(0.5)
  shinyjs::addClass(selector = paste0(
    "div.submissionTab>main>form>div.tabbable>div.tab-content>div.tab-pane:nth-child(", nexttab, ")"), 
    class = "fade-inL")
  
  # ADD EVENT LISTENER TO NEXT TAB TO SCROLL INTO VIEW WHEN ANIMATION ENDS
  js_code <- sprintf("
    var nextTab = document.querySelector('div.submissionTab > main > form> div.tabbable > div.tab-content > div.tab-pane:nth-child(%d)');
    if (nextTab) {
      nextTab.addEventListener('animationend', function() {
        nextTab.scrollIntoView({ behavior: 'smooth', block: 'start' });
      }, { once: true });
    }
  ", nexttab)
  
  shinyjs::runjs(js_code)
  
}

#TO ALIGN ELEMENTS NICELY WHEN A SCROLL BAR IS NEEDED, DT TABLES DRAW A FOOTER THAT IS A TABLE ELEMENT THAT IS EMPTY, WHICH MESSES UP SCREEN READERS. THIS FINDS THE EMPTY TABLE AND GIVES IT A PRESENTATION ROLE SO SCREEN READERS KNOW THE EFFECT IS ONLY VISUAL. IT ALSO HIDES IT VIA ARIA-HIDDEN. 
#THE EARLIER BIT IN HERE IS FROM CHATGPT. IT SETS EACH COLUMN HEADER AS A SEMANTIC HEADER CELL. THIS SHOULD HELP SCREEN READERS ASSOCIATE HEADERS WITH THE APPROPRIATE CELLS IN THE TABLE. OTHERWISE, THE TH ELEMENTS COULD BE ROW/COLUMN/BOTH HEADERS, SO A SCREEN READER WOULDN'T GUESS AND JUST WOULDN'T SAY THE HEADERS ALOUD AT ALL WHEN A USER NAVIGATES TO A CELL.
#IT ALSO DYNAMICALLY RECOGNIZES THE SORT DIRECTION FOR EACH COLUMN AND UPDATES THE ARIA-SORT ATTRIBUTE SO THAT A SCREEN READER WOULD SAY THE SORT DIRECTION WHEN A TABLE ROW IS HIGHLIGHTED. 
#ANOTHER THING IT DOES IS FIND ANY OF THE NUMBERED PAGINATION BUTTONS AND GIVES THEM ARIA LABELS THAT APPEND "PAGE" TO THE FRONT OF THE NUMBERS SO IT'S MORE OBVIOUS WHAT THEY'RE FOR FOR SCREEN READERS. 
DT_A11Y_Callback <- DT::JS('
  table.on("draw", function() {
    // Scope and sort attributes on header cells
    var headerCells = table.table().header().querySelectorAll("th");
    headerCells.forEach(function(th) {
      th.setAttribute("scope", "col");
      th.removeAttribute("aria-sort");

      if (th.classList.contains("sorting_asc")) {
        th.setAttribute("aria-sort", "ascending");
      } else if (th.classList.contains("sorting_desc")) {
        th.setAttribute("aria-sort", "descending");
      } else {
        th.setAttribute("aria-sort", "none");
      }
    });

    // Hide the visual-only footer table
    var footerTable = $(table.table().container()).find("div.dataTables_scrollFootInner table");
    if (footerTable.length > 0) {
      footerTable.attr("role", "presentation");
      footerTable.attr("aria-hidden", "true");
      if (footerTable.find("thead").length === 0) {
        footerTable.prepend(\'<thead><tr><th style="visibility: hidden;">This is not a real table.</th></tr></thead>\');
      }
    }

    // Add aria-label to numbered pagination buttons
    $(".dataTables_paginate a").each(function() {
      var $this = $(this);
      var text = $this.text().trim();
      if (!isNaN(text)) {
        $this.attr("aria-label", "Page " + text);
      }
    });
  });
')