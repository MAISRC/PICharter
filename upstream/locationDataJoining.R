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


# Establishing connections to the Google Drive structures -----------------

#GET ACCESS TO THE METADATA FILE FOR ITS FLAGS AND METADATA
metadata_id = googledrive::drive_get("https://docs.google.com/spreadsheets/d/1iqJThuAjoMhRs1njvWPi_7pbszM-Dlvl933L6wsPR6k/edit?usp=sharing")$id
metadata_sheet = googlesheets4::read_sheet(ss = metadata_id,
                                           sheet = "submitted_metadata")

#GET ACCESS TO THE LOCATION DATA SET FOLDER.
locdata_id = googledrive::drive_get("https://drive.google.com/drive/folders/14ASYFti6Yfjy3Bv2K7lpwQn5t9J46Y7P")$id
garbage_id = googledrive::drive_get("https://drive.google.com/drive/folders/1WIgmRVbY1mDedi-wZSQQUBOqadehWKyp")$id
archived_id_loc = googledrive::drive_get("https://drive.google.com/drive/folders/1aRdTI0jDyL0JoylaOQod5xHWRDVt-QAm")$id
cleandata_id_needloc = googledrive::drive_get("https://drive.google.com/drive/u/1/folders/1DpG5FCZka1sXx-ztuEOitVA8MpRdx5r4")$id
archived_id_clean = googledrive::drive_get("https://drive.google.com/drive/u/1/folders/1DgBorlz_n0RLPKCpQsA0OAlOgXt8UG_A")$id

# Convenience functions -------------------------------------------
tidyName = function(x) {
  base::tolower(
    stringr::str_replace_all(x,
                             pattern = "([\\.\\(\\)\\-\\/\\?])|([_]+)|([\\s]+)",
                             replacement = "_")
  )
}

#THIS FUNCTION IS MEANT TO HUNT THRU THE DOCUMENT TO FIND PROSPECTIVE COORDINATE DATA COLUMNS.
findGeom = function(df) {
  
  #IF IT'S AN SF OBJECT, IT HAS GEOMETRY DATA--EXIT EARLY
  if(inherits(x = df, what = "sf")) {
    return(TRUE)
  }
  
  (unname(vapply(df, function(x) {
    any(grepl("^\\s*POINT\\s*\\(", x), na.rm = T) | #THIS ASKS IF ANY OF THIS COLUMN LOOKS LIKE STANDARD WKT POINT FORMAT
    all(suppressWarnings(as.numeric(x)) >= -180 & 
        suppressWarnings(as.numeric(x)) <= 180 & #THIS ASKS IF THIS LOOKS LIKE STANDARD LAT OR LONG DATA
        suppressWarnings(as.numeric(x) %% 1 != 0), na.rm=T) #BUT THEN ISN'T JUST INTEGER DATA, LIKE STATION NUMBERS MIGHT BE.
      
      
  }, logical(1))) |
    names(df) %in% c("lon", "lat", "lng", "x", "longitude", "y", "latitude", "WKT", "wkt", "geom", "geometry", "wkt_str", "coord_text", "long_dd", "lon_dd", "utmx", "UTMX", "utmy", "UTMY")) & #THIS ASKS IF A COLUMN JUST HAS A NAME WE MIGHT RECOGNIZE AS BEING COORDINATE DATA.
    !names(df) %in% c("sta_nbr", "DOW", "SURVEY_START") #WE DON'T WANT THESE OTHER COLUMNS CONSIDERED.
  
}

#THIS TRIES TO DETERMINE IF A COLUMN IS WKT DATA AND THUS CAN BE ADEQUATELY CONVERTED TO LAT/LONG DATA AS A RESULT.
is_wkt = function(df) {
  
  coords = df %>% 
    select(coordinates)
  
  convert_try = st_as_sfc(coords$coordinates, crs = 4326)
  
  names(coords) %in% c("WKT", "wkt") |
    all(st_is(convert_try[!is.na(convert_try)], "POINT"))
  
}

#REMOVE THE MOST RECENT ENTRIES FROM THE WORKING GROUP OBJECTS. THIS PREVENTS US FROM HAVING TO QUERY THE GOOGLE DRIVE FILE LIST EVERY TIME.
remove_processed = function() {
  locdata_filenames <<- locdata_filenames[-1] #HARD-CODED TO MATCH CURRENT TEST STRUCTURE ****
  locdata_fileids <<- locdata_fileids[-1]
}

archiveloc = function(locfile){
  file_id = googledrive::drive_ls(pattern = locfile, path = as_id(locdata_id))$id[1]
  
  googledrive::drive_mv(
    file = file_id, 
    path = archived_id_loc, 
    overwrite = TRUE)
}

move_compiled_clean = function(metadata_row){

  file_name = metadata_sheet$CLEAN_FILE[metadata_row]
  file_id = googledrive::drive_ls(pattern = file_name, path = cleandata_id_needloc)$id[1]
  
  googledrive::drive_mv(
    file = file_id, 
    path = archived_id_clean, 
    overwrite = TRUE)
}

# Loading in support files ------------------------------------------------

#LOAD IN LOOKUP TABLE FOR DETERMINING WHICH COLS ARE RECOGNIZED, WHETHER THEY ARE TAXONOMIC.
fieldnames = utils::read.csv("inputs/Dynamic/column_name_lookup.csv") %>% 
  dplyr::mutate(newfieldname = tidyName(newfieldname)) %>%
  dplyr::select(fieldname, newfieldname) %>%
  dplyr::distinct()

stn_num_names = sort(fieldnames$fieldname[fieldnames$newfieldname=="sta_nbr"]) #STATION NUMBER NAMES WE'VE SEEN BEFORE.


# Manage current database file safely -------------------------------------

archival_path = "upstream\\Archived database summaries" #ARCHIVE FOLDER
archival_filename = paste0("\\picharter_dbnew", Sys.Date(), ".parquet") #NEW FILE NAME
current_dbname = "upstream\\db_unified.parquet" #CURRENT FILE'S NAME
current_inputdb = "inputs\\MadeUpstream\\db_unified.parquet"
current_db = read_parquet(current_dbname) #READ
write_parquet(as.data.frame(current_db), sink = paste0(archival_path, archival_filename)) #WRITE

# Identify prospective records needing joining ----------------------------

#CONDUCT A SURVEY OF THE LOCATION DATA FOLDER UPON STARTUP
locdata_filenames = googledrive::drive_ls(path = locdata_id)$name
locdata_fileids = googledrive::drive_ls(path = locdata_id)$id

#EXCLUDE GARBAGE FOLDER
locdata_filenames = locdata_filenames[locdata_filenames != "garbage"]
locdata_fileids = locdata_fileids[locdata_fileids != garbage_id]

#EXCLUDE ARCHIVED FOLDER
locdata_filenames = locdata_filenames[locdata_filenames != "Archived"]
locdata_fileids = locdata_fileids[locdata_fileids != archived_id_loc]

#CHECK THE METADATA FOLDER FOR FILES NEEDING THEIR LOCATION DATA JOINED. THEY'LL HAVE AN L IN THE METADATA FILE, A FILE LISTED IN THE LOC_FILE COLUMN, AND A MATCHING FILE IN THE LOCDATA FOLDER.
prop_records = which(grepl("L", metadata_sheet$APPROVED_BY_ALEX) &
                     !is.na(metadata_sheet$LOC_FILE) &
                     metadata_sheet$LOC_FILE %in% locdata_filenames
)

#WE WANT TO EXCLUDE ANY NEW SUBMISSIONS THAT DON'T YET HAVE APPROVAL BUT WOULD BE IN THE METADATA SHEET AND LOC FOLDER.
records_under_consideration = metadata_sheet$LOC_FILE[prop_records]
records_nums = which(locdata_filenames %in% records_under_consideration)

#EXCLUDE
locdata_filenames = locdata_filenames[records_nums]
locdata_fileids = locdata_fileids[records_nums]

#length(prop_records) #AT TIME OF WRITING, 325. 

#WHAT RANGE OF FILE TYPES ARE WE LOOKING AT?
#unique(tools::file_ext(locdata_filenames)) #EXCELS, CSVS, ZIPS, AND GPS FILES.


# LOADING CURRENT FILE -----------------------------------------------------------------

#LET'S EXAMINE THE FIRST (LEAST RECENT) AVAILABLE CSV FILE.
# this_file = tail(which(tools::file_ext(locdata_filenames) == "csv"), 1)
this_file = 1

(this_filename = locdata_filenames[this_file])

new_filepath = paste0("upstream/tempfiles/", this_filename)

googledrive::drive_download(file = locdata_fileids[this_file], overwrite = TRUE, path = new_filepath)

##BRANCH ACCORDING TO FILE TYPE

if(tools::file_ext(this_filename) == "csv") {

test_locfile = read.csv(new_filepath)

}

if(tools::file_ext(this_filename) == "zip") {
  
  file_list = utils::unzip(zipfile = new_filepath, list = T) #GET FILE LIST INSIDE ZIP FOLDER
  
  shp_files = which(grepl("\\.shp$", file_list$Name))
  
  if(length(shp_files) == 0) {
    stop("This location file submission is a zip file, but it does not contain any shape files!")
  }
  
  if(length(shp_files) > 1) {
    stop("This location file submission is a zip file, but it contains too many shape files!")
  }
  
  shp_name = file_list$Name[shp_files]
  
  utils::unzip(zipfile = new_filepath, overwrite = T, exdir = "upstream/tempfiles/zips")
  
  test_locfile = st_read(paste0("upstream/tempfiles/zips/", shp_name))
  
}

View(test_locfile) #SHOW THE FILE FOR CONVENIENCE

# Early failures -------------------------------------------------------

#THIS ALGORITHM HAS A COUPLE OF HAPPY PATHS. REACHING THEM IS CONTINGENT ON A FEW THINGS:
#A) BEING ABLE TO OPEN THE LOCATION FILE (NOT AN ISSUE HERE, BUT COULD BE ONE IN THE APP)
#B) THE CORRESPONDING SURVEY DATA BEING IN THE DATABASE (UNLIKELY TO BE A PROBLEM BUT COULD BE, BUT TOTALLY INAPPLICABLE IN THE APP)
#C) THERE BEING A FINDABLE STATION NUMBER COLUMN (OR) THERE BEING AN EQUAL NUMBER OF RECORDS IN THE SURVEY FILE AND IN THE LOCATIONS FILE FOR 1-TO-1 MATCHING
#D) THERE BEING VIABLE COORDINATE/LOCATION DATA IN THE LOCATION FILE TO JOIN IN.

#SO, THE IDEA IS TO PERFORM AS MANY CHECKS AS POSSIBLE UPFRONT TO "FAIL EARLY" IF NONE OF THE HAPPY PATHS CAN BE REACHED. IF ANY OF THE CHECKS REQUIRE SIGNIFICANT OPERATIONS, WE ORDER THOSE TO COME LATER SO WE ONLY BEAR THE COST IF WE MUST. IF SOME CHECKS WOULD REQUIRE SUBSTANTIAL OPERATIONS THAT WOULD HAPPEN ALONG A HAPPY PATH ANYWAY, WE CAN VIEW THOSE AS SUNK COSTS THAT WE CAN BEAR UP FRONT. OTHERWISE, PUNT THOSE ABORTS INTO THE HAPPY PATHS TO AVOID NEEDLESS COMPUTATION.

#SUNK COST OPERATIONS--BINDING IN DOW AND SURVEY_START DATA
#WE NEED THESE FOR CHECKS, AND WE'D NEED THEM TO LINK UP THE EVENTUAL DATA TOO.
right_row = which(metadata_sheet$LOC_FILE == this_filename)

#FAIL IF WE CAN'T GET A MATCH BECAUSE THE SUBMITTED LOCATION FILE IS A RELIC/DERELICT REMNANT OF A DELETED SUBMISSION.
if(length(right_row) == 0) {
  stop("This file is in the submitted_locdata folder but not in the metadata file--it might be a relic that could be deleted!")
}

#BIND TO LOCATION FILE A DOW AND SUREVEY START DATE FROM THE METADATA SHEET.
test_locfile$DOW = metadata_sheet$DOW[right_row]
test_locfile$SURVEY_START = str_sub(metadata_sheet$SURVEY_DATE[right_row][[1]], 1, 11) #WEIRD FORMATTING NEEDED TO GRAB EXACTLY WHAT WE WANT HERE.

#CHECK 1 (ITEM B) -- CAN WE FIND THE RECORDS FOR THIS SURVEY IN THE DATABASE? IF SO, SHOULD BE ABLE TO FIND ROWS IN DB WITH MATCHING DOW AND SURVEY_START INFO.
db_data = which(as.character(current_db$DOW) == as.character(test_locfile$DOW[1]) & 
                  as.character(current_db$SURVEY_START) == as.character(test_locfile$SURVEY_START[1])) #ROWS IN THE DB FILE

if(length(db_data) == 0) {
  print("This survey is not apparently in the database under these DOW and SURVEY_START data!")
  ##FAIL EARLY
}

#CHECK 2 (ITEM D) -- CAN WE FIND OBVIOUS COORDINATE COLUMN(S)? IF NOT, THE FILE IS NOT HELPFUL.
{print("Coordinates check: ")
(coord_check = any(findGeom(test_locfile)))}
#CHECK 2 IS ALONE INSUFFICIENT TO FORCE AN ABORT--THERE COULD BE NON-OBVIOUS COORDINATE DATA. BUT IT REPRESENTS A BIFURCATION POINT THAT MIGHT LEAD TO FAILURE.

#CHECK 3 (ITEM C) -- CAN WE FIND AN OBVIOUS STATION NUMBER COLUMN (I.E., IT EXISTS ALREADY IN OUR GLOBAL COLUMN NAMES LOOKUP TABLE)?
{print("Prospective station numbers check: ")
(prospective_stanbrs = which(names(test_locfile) %in% stn_num_names))}
#CHECK 3 IS ALONE INSUFFICIENT TO FORCE AN ABORT--THERE COULD BE A NON-OBVIOUS STATION NUMBER NAME OR ELSE THE LOCATION DATA AND SURVEY DATA COULD BE THE SAME LENGTH. BUT IT REPRESENTS A BIFURCATION POINT THAT MIGHT LEAD TO FAILURE.

#WE CAN'T CHECK EQUAL LENGTH OF THE TWO DATA SETS YET BECAUSE WE HAVE NOT CLEANED THE LOCATION DATA--WE'LL BEAR THAT SUNK COST LATER.


#FROM HERE, TO ASCERTAIN IF THE FILE IS VIABLE, IF WE HAVE FAILED TO FIND VALID AND UNAMBIGUOUS STATION NUMBERS, WE HAVE TO PROMPT THE USER TO LOOK AND SEE IF THERE ARE NON-OBVIOUS COLUMNS CONTAINING THESE DATA.
if(length(prospective_stanbrs) != 1) {
  
  print(head(test_locfile))
  new_stanbrs = readline("
The algorithm did not find a single umabiguous prospective station number column.
Does any column look like a station number column? 
Press Y if yes and any other key for no.")
  
  #FLAG TRIGGER--THERE NOT BEING A NON-OBVIOUS STATION NUMBERS COLUMN IS STILL INSUFFICIENT TO TRIGGER AN ABORT, AS WE COULD HAVE EQUAL NUMBERS OF RECORDS, BUT WE CAN FLAG IT FOR A POST-CLEANING CHECK.
  if(new_stanbrs != "Y") {
    no_sta_nbrs_flag = TRUE
  } else {
    
  #OTHERWISE, IDENTIFY THE NEW STATION NUMBER COLUMN
  print("Survey record primary station numbers: ")
  print(sort(unique(current_db$sta_nbr[db_data])))
  print("Location data file column names: ")
  print(names(test_locfile))
  id_new_stanbrs = readline("
  What is the name of the (right) station number column?
  Please enter it here EXACTLY as it is shown above.
  There may be multiple viable candidates. Make your best guess.")
  
  #FAIL UPON INVALID INPUT.
  if(!id_new_stanbrs %in% names(test_locfile) |
     length(id_new_stanbrs) != 1) {
    stop("You goofed! The column name you indicated is not one of those in the location file! Or you failed to provide exactly one column name!")
  }
  
  #OTHERWISE, RENAME THAT COLUMN STA_NBR AND END
  names(test_locfile)[names(test_locfile) == id_new_stanbrs] = "sta_nbr"
  no_sta_nbrs_flag = FALSE #SET FLAG EITHER WAY 
  tmp1 = readline("
That column has been relabeled as station number.
Now is a good time to add its original name to the lookup table.
Press any key to continue.")

  }
} else {
  
  names(test_locfile)[prospective_stanbrs] = "sta_nbr"
  no_sta_nbrs_flag = FALSE #SET FLAG EITHER WAY 
  tmp1 = readline("
A prospective station number column has been identified and labeled. 
Now\'s a good time to check to make sure this went ok. 
Press any key to continue.")
  
}

#IS THE LOCATION DATA FILE AN SF FILE? IF SO, THOSE ARE ANNOYING--LET'S CONVERT.
{print("SF file check: ")
(sf_file_yes = inherits(test_locfile, "sf"))}

if(sf_file_yes) {
  epsg = st_crs(test_locfile)$epsg #GET THE EPSG CODE
  
  if(is.na(epsg) || epsg != 4326) { #IF NOT OBVIOUSLY THE STANDARD LAT-LONG DATA, CONVERT
    test_locfile = st_transform(test_locfile, crs = 4326)
  }
  
  test_locfile = test_locfile %>% 
    mutate(longitude = st_coordinates(.)[, "X"],
           latitude = st_coordinates(.)[, "Y"]) %>% 
    st_drop_geometry()
  print("SF file geometry was converted to lat/long columns/CRS and the old geometry was dropped.")
  
}

#CHECK 4 (ITEM D) -- ARE THERE ALREADY COLUMNS CALLED EXACTLY latitude AND longitude?

latlong_valid_check = FALSE #DEFAULT IS FAILURE

{print("Lat/long data present check: ")
(latlong_check = all(c("latitude", "longitude") %in% names(test_locfile)))}

#CHECK 5 (ITEM D) -- IF SO, ARE THEY WITHIN VALID RANGES? SOME FOLKS SEEM TO MIS-NAME THESE COLUMNS. 
if(latlong_check) {
  
lat_valid_check = all(test_locfile$latitude > 43.5 &
                        test_locfile$latitude < 49.38)

long_valid_check = all(test_locfile$longitude > -97.24 &
                        test_locfile$longitude < -89.49)

if(lat_valid_check & long_valid_check) {
  latlong_valid_check = TRUE #ONLY OVERRULE ON SUCCESS.
  print("The lat/long data present appear to be valid!")
} else {
  print("The lat/long data present appear to be invalid!")
}
} 

  
#THEN, IF NEEDED, CHECK FOR NON-OBVIOUS GEOMETRIES WITH THE USERS, EXCEPT THERE MAY BE MORE THAN ONE SUCH COLUMN, IN WHICH CASE WE HAVE TO ACCOMMODATE THAT POSSIBILITY. BASICALLY, IF THERE AREN'T EXACTLY TWO COLUMNS CALLED LATITUDE AND LONGITUDE THAT CONTAIN THE RIGHT DATA, WE HAVE MORE WORK TO DO. 
if(coord_check == FALSE | #NO OBVIOUS COORD DATA
   (coord_check == TRUE & #OR THERE IS BUT IT'S NOT VALID LAT/LONG
    latlong_valid_check == FALSE)
   ) {
  
  print(head(test_locfile))
  new_coords = readline("The algorithm did not find 1+ prospective coordinates column(s) containing data in the right format.
                         Do(es) any column(s) look like it contains coordinate data of any valid form?
                         Press Y if yes. Press any other key for no.")
  
  #FAIL EARLY--NO COORDINATE DATA MEANS THIS LOCATION FILE IS WORTHLESS
  if(new_coords != "Y") {
    
    readline("This location file is non-viable because *it does not contain any apparent, properly formatted coordinate data*. Delete it from the submitted_locdata folder, remove it from the metadata file, and contact the submitter to inform them.Press any key to continue.")
    ##STOP/NEXT/BREAK COMMAND?
    
  } else {
  
  #OTHERWISE, IDENTIFY THE NEW COORDINATE COLUMNS
  print(names(test_locfile))
  id_new_coords = readline("What\'s the name of the coordinate data column(s)? 
                           Favor lat/long over UTM. 
                           Enter ONE or TWO column names EXACTLY as shown above. 
                           Delimit with \", \".")
  
  #SPLIT UPON THE DELIMITER
  id_new_coords_split = str_split_1(id_new_coords, ", ")
  
  #FAIL UPON INVALID INPUT.
  if(!all(id_new_coords_split %in% names(test_locfile)) |
     length(id_new_coords_split) == 0 |
     length(id_new_coords_split) > 2) {
    stop("You goofed! You either specified non-existant column names for the coordinate columns or else you specified an invalid number of them!")
  }
  
  #OTHERWISE, RENAME ALL COLS TO COORDINATES1 (+ OPTIONALLY COORDINATES2) AND CLOSE.
  names(test_locfile)[names(test_locfile) == id_new_coords_split[1]] = "coordinates1"
  
  if(length(id_new_coords_split) == 2) {
    names(test_locfile)[names(test_locfile) == id_new_coords_split[2]] = "coordinates2" 
  }
  
  tmp1 = readline("
  The specified column(s) got renamed as coordinates.
  Now is a good time to add the original name(s) to the lookup table, if appropriate.
  Press any key to continue.")
  }
}

#CHECK #6 (ITEM C) -- DO THE STATION NUMBERS (IF PRESENT) HAVE ANY DUPLICATES? IF SO, THAT WOULD SUGGEST THEY MAYBE AREN'T VALID.
if("sta_nbr" %in% names(test_locfile)) {
any_duped_stanbrs = if(isTRUE(any(duplicated(test_locfile$sta_nbr)))) {TRUE} else {FALSE}

#CHECK 6B--I'VE SEEN AT LEAST ONE INSTANCE WHERE THE ID COLUMN WAS ALL 0S. THIS WOULD TRIGGER FAULTY BEHAVIOR LATER, SO WE CHECK FOR IT HERE. 
if(any_duped_stanbrs) {
  if(length(unique(test_locfile$sta_nbr)) == 1) {
    no_sta_nbrs_flag = TRUE #OVERRIDE THIS FLAG TO INDICATE, ACTUALLY, WE DON'T HAVE A FUNCTIONING STA_NBR COLUMN AFTER ALL
    test_locfile = test_locfile %>% 
      select(-sta_nbr)
    print("The station numbers present seem to be invalid!")
  }
 } else { 
  any_duped_stanbrs = FALSE 
  print("The station numbers present seem to be valid!")
 }
}


##BASIC CLEANING (SUNK COST + PREP FOR NEXT CHECKS)
##AT THIS STAGE, WE'VE EITHER IDENTIFIED COORDINATE DATA OR ENDED, SO WE CAN ASSUME SUCH DATA EXIST AND THUS WE CAN CLEAN THE DATA SET BASED ON THAT.
#WE CAN START BY FILTERING OUT ANY ROWS THAT DO NOT CONTAIN COORDINATE DATA
test_locfile2 = test_locfile %>%
  filter(if_all(starts_with("coordinates"), ~ !is.na(.x) &
                  .x != "" &
                  .x != " " &
                  .x != 0)) %>% 
  filter(if_all(latitude, ~ !is.na(.x) &
           .x != "" &
           .x != " " &
           .x != 0)) %>% 
  filter(if_all(longitude, ~ !is.na(.x) &
           .x != "" &
           .x != " " &
           .x != 0))

#IF THERE'S A STA_NBR COLUMN NOW, THEN DO THE SAME FOR STA_NBR VALUES 
if(!no_sta_nbrs_flag) {
  test_locfile2 = test_locfile2 %>%
    filter(!is.na(sta_nbr) &
             sta_nbr != "" &
             sta_nbr != " " #&
             #sta_nbr != 0 #<--WE CAN ONLY FILTER OUT 0S IF WE'RE SURE THE STATION NUMBERS DO NOT BEGIN WITH A 0!
             )
}


#CHECK 7 (ITEM D) -- IS THERE AN EQUIVALENT NUMBER OF ROWS IN BOTH DATA STRUCTURES? THIS IS ONLY RELEVANT IF THERE WERE NO STATION NUMBERS. IF THIS CHECK FAILS, IT'S A FAIL STATE. 
{print("Equivalent numbers of rows check: ")
(equal_recordsN_check = nrow(test_locfile2) == length(db_data))}

if(no_sta_nbrs_flag &
   equal_recordsN_check == FALSE) {
  
  #PRINT METADATA FOR REPORTING TO USER. 
  print(this_filename)
  print(paste0("This metadata row: ", right_row + 1))
  
  current_db[db_data, ] %>%
    dplyr::select(where(~ !all(is.na(.x)))) %>%
    write.csv(file = "upstream/tempfiles/PICharter_surveyRecord_invalidLocData.csv", row.names = FALSE) # PRODUCE A REFERENCE DOC FOR THE USER IN CASE THEY WANT TO TRY AGAIN. 
  remove_processed()
  tmp1 = readline("
   This location file is invalid because *it contains no valid sta_nbrs
   and is not equal in length to the survey records*.
   Delete it from submitted_locdata, remove it from 
   metadata, archive the clean file, and contact the submitter.")
  ##STOP/NEXT/BREAK COMMAND?
  
}


##BY THIS POINT, WE NEED TO NO LONGER HAVE COORDINATES COLUMNS BUT LAT/LONG COLUMNS!!! ****

#VIABLE STATION NUMBERS AND COORDINATE DATA SEEM TO EXIST
if(!no_sta_nbrs_flag){ 
  
  #FAILSAFE IF WE MIGHT ACCIDENTALLY OVERWRITE ANY LAT/LONG DATA
  if(any(!is.na(current_db[db_data, "latitude"])) |
     any(!is.na(current_db[db_data, "longitude"]))) {
    stop("Wait, there are already some lat/long data here!")
  }
  
  #HAPPY PATH 1--ALL THE STATION NUMBERS IN THE LOCATION FILE MATCH ALL THE STATION NUMBERS IN THE SURVEY DATA
  {print("All station numbers match check: ")
  do_all_stnnums_match = length(db_data) == nrow(test_locfile2) && 
    all(sort(as.character(current_db[db_data, ]$sta_nbr)) == sort(as.character(test_locfile2$sta_nbr)))
  print(do_all_stnnums_match)}
  
  if(do_all_stnnums_match) {
    
    sta_nbrs2match = as.character(current_db[db_data, ]$sta_nbr) #GET ORDER OF CURRENT STA_NBRS IN CASE THEY ARE OUT OF ORDER.
    
    tmp1 = test_locfile2 %>% 
      arrange(factor(sta_nbr, levels = sta_nbrs2match)) #RESORT LOCATION DATA TO SAME ORDER AS DB.
    
    #CART OVER THE COORDINATE DATA
    current_db[db_data, "latitude"] = tmp1$latitude
    current_db[db_data, "longitude"] = tmp1$longitude
    
    write_parquet(as.data.frame(current_db), sink = "upstream/db_unified.parquet")
    file.copy(from = current_dbname, to = current_inputdb, overwrite = T)
    remove_processed()
    print(this_filename)
    print(paste0("Metadata row: ", right_row + 1))
    archiveloc(this_filename)
    move_compiled_clean(right_row)
    tmp2 = readline("
The coordinate data were successfully synced!
The location data and cleaned data files have been moved and archived.
Update the Approval Code in the metadata file to C from L. 
Press any key to continue.")
    
    ##SOME SORT OF NEXT COMMAND HERE##
    
  }
  
  ##HAPPY PATH 2, IS THE LIST OF STATION NUMBERS IN THE LOCATION FILE MORE COMPREHENSIVE THAN THAT IN THE DATA FILE? IF SO, THAT WOULD PROBABLY BE OK--IT WOULD JUST REFLECT THE FACT THAT THERE WERE SOME NON-THROWS, MOST LIKELY.
  {print("All station numbers are comprehensive check: ")
  loc_dat_comprehensive = all(as.character(current_db[db_data, ]$sta_nbr) %in% as.character(test_locfile2$sta_nbr))
  print(loc_dat_comprehensive)}
  
  if(loc_dat_comprehensive){
    
    sta_nbrs2match = as.character(current_db[db_data, ]$sta_nbr) #GET ORDER OF CURRENT STA_NBRS IN CASE THEY ARE OUT OF ORDER OR MISSING.
    tmp1 = test_locfile2 %>% 
      filter(sta_nbr %in% sta_nbrs2match) %>% #FIRST, FILTER OUT ANY THAT HAVE NO MATCHES IN THE DB DATA.
      arrange(factor(sta_nbr, levels = sta_nbrs2match)) #THEN, RESORT LOCATION DATA TO SAME ORDER AS DB.
    
    if(nrow(tmp1) != length(db_data)) {
      stop("We're about to match data of unequal lengths somehow!")
    }
    
    #CART OVER THE COORDINATE DATA
    current_db[db_data, "latitude"] = tmp1$latitude
    current_db[db_data, "longitude"] = tmp1$longitude
    
    write_parquet(as.data.frame(current_db), sink = "upstream/db_unified.parquet")
    file.copy(from = current_dbname, to = current_inputdb, overwrite = T)
    remove_processed()
    print(this_filename)
    print(paste0("Metadata row: ", right_row + 1))
    archiveloc(this_filename)
    move_compiled_clean(right_row) #<--NO +1, DIFF BTW. COMPUTER-READ AND HUMAN-EYE-READ ROWS!
    tmp2 = readline("
The coordinate data were successfully synced!
The location data and cleaned data files have been moved and archived.
Update the Approval Code in the metadata file to C from L.
Press any key to continue.")
    
  }
  
  ##HAPPY PATH 3: ARE THERE SECONDARY STATION NUMBER DATA FOR THIS SURVEY THAT WE MIGHT MATCH?
  {print("Secondary station number check: ")
  (secondary_stanbrs = all(!is.na(current_db$sta_nbr.1[db_data])))}
  
  if(secondary_stanbrs){
    
    {print("All secondary station numbers are comprehensive check: ")
    (loc_dat_comprehensive2 = all(as.character(current_db[db_data, ]$sta_nbr.1) %in% as.character(test_locfile2$sta_nbr)))
    }
    
    #PROCEED IF GOOD. SAME AS ABOVE.
    if(loc_dat_comprehensive2) { #<--
      
      sta_nbrs2match = as.character(current_db[db_data, ]$sta_nbr.1) #<-- 
      
      tmp1 = test_locfile2 %>% 
        filter(sta_nbr %in% sta_nbrs2match) %>% 
        arrange(factor(sta_nbr, levels = sta_nbrs2match))
      
      #CART OVER THE COORDINATE DATA
      current_db[db_data, "latitude"] = tmp1$latitude
      current_db[db_data, "longitude"] = tmp1$longitude
      
      write_parquet(as.data.frame(current_db), sink = "upstream/db_unified.parquet")
      file.copy(from = current_dbname, to = current_inputdb, overwrite = T)
      remove_processed()
      print(this_filename)
      print(paste0("Metadata row: ", right_row + 1))
      archiveloc(this_filename)
      move_compiled_clean(right_row)
      tmp2 = readline("
The coordinate data were successfully synced!
The location data and cleaned data files have been moved and archived.
Update the Approval Code in the metadata file to C from L.
Press any key to continue.")
    }
  }
  
  
}

#POTENTIAL HAPPY PATH: NO STATION NUMBERS, BUT EQUAL NUMBERS OF RECORDS




# 
# #OK, KNOWING THAT THE COORDINATES IN THIS CASE ARE WKT FORMAT, WE CAN WRITE CODE THAT TRIES TO CONVERT THEM BASED ON THAT. THIS WOULD REQUIRE BRANCHING LOGIC FOR OTHER FORMATS.
# if(is_wkt(test_locfile2)) {
#   
#   geom = st_as_sfc(test_locfile2$coordinates, crs = 4326)
#   
#   if(!all(st_is(geom, "POINT") | st_is_empty(geom))) {
#     stop("Something in here isn't a POINT geometry.")
#   }
#   
#   coords = st_coordinates(geom)
#   
#   test_locfile3 = test_locfile2 %>% 
#     mutate(longitude = coords[, "X"],
#            latitude = coords[, "Y"]) %>% 
#     select(-coordinates)
#   
# }
# 
# 
# test_locfile3 #LOOKS LIKE A SUCCESSFUL CONVERSION!
