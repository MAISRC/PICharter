#READ LOCATION OF LOCATION DATA FILES
locdata_id = googledrive::drive_get("https://drive.google.com/drive/folders/14ASYFti6Yfjy3Bv2K7lpwQn5t9J46Y7P")$id

current_dbname = "H:\\Shared drives\\MAISRC\\Quantification, Data, and Computation\\Projects\\Statewide Plant Surveys App\\PI Charter\\upstream\\db_unified.parquet" #CURRENT FILE'S NAME
current_db = read_parquet(current_dbname)

#READ CONTENTS OF FOLDER
locfile_contents = drive_ls(locdata_id)

#ASSESS RANGE OF FILE TYPES
unique(file_ext(locfile_contents$name)) #There are zips, xlsx, xls, csv, and gpx files.

#SEPARATE INTO FILE TYPES--THE VAST MAJORITY ARE ZIPS
locfile_csvs = locfile_contents[grepl(".csv", locfile_contents$name),]
locfile_xls = locfile_contents[grepl(".xls", locfile_contents$name),]
locfile_gpss = locfile_contents[grepl(".gpx", locfile_contents$name),]
locfile_zips = locfile_contents[grepl(".zip", locfile_contents$name),]

#GET ACCESS TO THE METADATA FILE FOR ITS FLAGS AND METADATA
metadata_id = googledrive::drive_get("https://docs.google.com/spreadsheets/d/1iqJThuAjoMhRs1njvWPi_7pbszM-Dlvl933L6wsPR6k/edit?usp=sharing")$id
metadata_sheet = googlesheets4::read_sheet(ss = metadata_id,
                                           sheet = "submitted_metadata")

#GO THRU EACH CSV FILE TO SEE IF IT CONTAINS DATA THAT CAN BE JOINED.
for(a in 1:nrow(locfile_csvs)) {
  
  current.loc = locfile_csvs[a, ]
  
  tmpdir = tempdir()
  drive_download(as_id(current.loc$id), 
                 path = paste0(tmpdir, "\\surveyfile", a), 
                 overwrite = T)
  
  current.dat <- read.csv(paste0(tmpdir, "\\surveyfile", a))
  
  metadata.row = metadata_sheet[as.character(metadata_sheet$LOC_FILE) == as.character(current.loc$name) & !is.na(metadata_sheet$LOC_FILE),]
  
  current.dow = metadata.row$DOW
  current.survey = as.character(metadata.row$SURVEY_DATE[[1]][1])
  
  db.dat = current_db %>% 
    filter(DOW == current.dow, SURVEY_START == current.survey)
  
  
}