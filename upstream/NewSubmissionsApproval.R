#THIS FILE WALKS THRU THE SUBMISSION QA/QC PROCESS USING A BUNCH OF CHECKSUMS AND FLAGS. IN MANY INSTANCES, THE FILE CAN BE REPAIRED AND RESAVED AUTOMATICALLY. MORE COMPLEX FIXES WILL TRIGGER STOPS. AT THE END, THE FILE WRITES THE RESULT INTO THE DATABASE FILE. IT DOES ***NOT*** ADD THESE NEW FILES TO THE HIVE STRUCTURE ON GDRIVE THOUGH. THAT I THINK MAKES SENSE TO DO SEPARATELY IN A NEW SCRIPT SO IT CAN BE DONE ONCE, SINCE ACCESSING THE GDRIVE IS SLOW.

#NOTE: THE CREATINGLAKESUMMARIES.R FILE WOULD STILL NEED TO BE RUN AFTER APPROVING A NEW SUBMISSION TO MAKE THE APP FULLY UP TO DATE! ALSO, FILES WOULD NEED TO BE ADDED TO THE GDRIVE HIVE--WHICH I THINK MAY STILL BE SAFER TO DO ELSEWHERE, SINCE MESSING WITH THAT IS SLOWISH. 

#Source the following file once per approval round!
#source("upstream/NewApprovalsPreLoad.R")

# Load, process, QA/QC, and compile approved submissions --------

#FIND ALL AVAILABLE NEW SUBMISSIONS, LIST, PREPARE TO LOOP OVER.
all_subs = drive_ls(submitted_clean_id)
content.ids = all_subs$id
content.names = all_subs$name
content.ids = content.ids[!as.character(content.ids) %in%  #IGNORE IRRELEVANT CONTENTS OF THIS FOLDER
                            as.character(c(already_approved_id, need_fixes_id))]
content.names = content.names[!as.character(content.names) %in%
                            as.character(c(already_approved_name, need_fixes_name))]

#PREVENT CONTINUING IF DUPES EXIST IN CLEAN DATA FOLDER.
if(any(duplicated(content.names))) {
  stop("Why are there duplicate files in the clean data folder?")
}

grow.dat = NA #START THIS AS NA BUT EXISTANT FOR FUTURE CHECKS. 

#LOOP OVER CLEAN FILES
for(n in 1:length(content.ids)) {

  print(paste0("Now on ", n, " of ", length(content.ids))) #PROGRESS

  id <- content.ids[n] #CURRENT FILE ID
  name <- content.names[n] #CURRENT FILE NAME
  metadata_row = metadata_sheet[metadata_sheet$CLEAN_FILE == name,] #WHERE IS THIS FILE, WHAT'S IT CALLED.
  raw_file_name = metadata_row$RAW_FILE

  #READ FILE
  current.import = drive_read_string(id, encoding = "UTF-8") %>% 
    read.csv(text = .)
  
  #PREPROCESSING STEPS--SKIP IF THEY'VE ALREADY OCCURRED (SUBMITTER_NAME WILL BE FIRST)
    if(names(current.import)[1] != "SUBMITTER_NAME") {
      
  #REPLACE ALL BLANKS W/ NAS
  current.import[current.import == ""] = NA
  
  current.import = current.import %>% #MOVE ALL METADATA TO FAR LEFT. DEETS-->https://stackoverflow.com/questions/22286419/move-a-column-to-first-position-in-a-data-frame
    select(SUBMITTER_NAME, SUBMITTER_EMAIL, DOW, SURVEY_START, 
           RAKE_MAX, SUBMIT_TIME, SURVEYORS, everything()) %>% 
    filter(rowMeans(is.na(.)) < 1) #REMOVE ANY NA ROWS
  
  #DELETE ALL EMPTY COLUMNS
  cols2cut = which(sapply(current.import, function(x) { as.logical(all(is.na(x) | x == 0)) }))
  names(cols2cut) = NULL
  current.import = current.import %>% select(-all_of(cols2cut))
  
  #DELETE ALL EMPTY ROWS
  rows2cut = apply(current.import, 1, function(x) { 
    as.logical(all(is.na(x) | x == 0))
    })
  current.import = current.import %>% filter(!rows2cut)
  rewritetoG()
  
  #REFERENCE PRINTING
  print(paste0("Raw file: ", raw_file_name))
  print(paste0("Clean file: ", metadata_row$CLEAN_FILE))
  open_raw = readline("The raw file for this submission is listed above. Pause here to open it, then hit any key to continue.")
  }
  
  #FIND TAXONOMIC COLS IN CURRENT SUB
  just_taxonomic = newfieldnames %>%  #FILTER TO ONLY TAXONOMIC COL NAMES
    filter(taxonomic == "Y")
  adjusted_names = names(current.import) #WE NEED TO REMOVE .1S AND .2S AS NEEDED BEFORE THIS CHECK
  adjusted_names = gsub(".1", "", adjusted_names)
  adjusted_names = gsub(".2", "", adjusted_names)
  taxonomic_cols = which(adjusted_names %in% just_taxonomic$newfieldname) #TAXONOMIC COLS
  
  #POP UP VIEWER OF CURRENT SUB FOR REFERENCE
   View(current.import)
  
  ##CHECKSUMS AND STOPS FOR CURRENT SUBMISSION. --------------------------
   

  ##CHECKING FOR SURVEY DUPLICATION
  currentdow = current.import$DOW[1] #GET DOW
  currentsurveydate = current.import$SURVEY_START[1] #GET SURVEY DATE
  if(length(which(current_db$DOW == currentdow &  #LOOK FOR THESE SAME VALS IN THE DB
                  current_db$SURVEY_START == currentsurveydate)) > 0) {
    stop("There's evidence that this is a duplicate survey record. Please verify before continuing submission.")
  } else {
    if(isTruthy(grow.dat) &&
       length(which(grow.dat$DOW == currentdow &
                    grow.dat$SURVEY_START == currentsurveydate)) > 0) {
      stop("There's evidence that this is a duplicate survey record (within the current submission). Please verify before continuing submission.")
    }
  }
  
  ##CHECK TO ENSURE THIS IS A RECOGNIZED DOW
  if(!ifelse(nchar(current.import$DOW[1]) == 7, yes = paste0("0", current.import$DOW[1]), no = as.character(current.import$DOW[1])) %in% all_DOWs) { #NEED TO REPAIR THE DOW IF NEEDED TO MATCH PROPERLY.
    dow_reccheck = readline("The DOW listed (first) in the current submission is not a recognized DOW value according to the DNR's database.\nShould we override the current DOW with a different value?\nPress Y for yes.\nPress N for no.\nPress any other key to stop.")
    if(dow_reccheck == "Y") {
      new_dow = readline("Type the new DOW value for this survey.")
      current.import$DOW = new_dow
      rewritetoG()

    } else {
      if(dow_reccheck != "N") {
        stop("The DOW of the current submission is invalid--please advise.")
      }
    }
  }
   
  ##FINDING AND RENAMING ANY AMBIGUOUS COLUMNS
  if(any(grepl("ambiguous", names(current.import), ignore.case = TRUE))) {
    ambi_names = which(grepl("ambiguous", names(current.import), ignore.case = T))
    for(i in ambi_names) {
      print(names(current.import)[i])
      rename_ambicol = readline("The above column has a name containing the 'ambiguous' substring.\nDo you want to rename this column?\nPress Y for yes,\nPress D to delete the column.\nPress any other key to continue.")
      if(rename_ambicol == "Y") {
        new_ambicol_name = readline("Type the new column name for this column.")
        names(current.import)[i] = new_ambicol_name #RENAME COL
        rewritetoG()
      } else {
        if(rename_ambicol == "D") {
          current.import = current.import[,-i] #DELETE COL
          rewritetoG()
        }
      }
    }
  }

  ##GOING THRU THE SUBMISSION METADATA
  
  #NON-THROWS AND NON-THROWS VALS
  if(metadata_row$NON_THROWS != "No selection") {
    print(metadata_row$NON_THROWS) #WHAT DID USER PUT, IF THEY USED IT
    print(metadata_row$NON_THROWS_VAL)
    nonthrows_check = readline("The user used the non-throws removal features and specified the non-throws column and non-throws values noted above.\nDo we need to stop and act?\nPress Y for yes,\nPress any other key to continue.")
    if(nonthrows_check == "Y") {
      stop("We need to address an issue related to non-throws in the current submission.")
    }
  }
  
  metadata_row[metadata_row == ""] = NA
 
  #LOOK_COMMENTS FLAG
  if(!is.na(metadata_row$LOOK_COMMENTS) |
    stringr::str_sub(metadata_row$COLUMNS_DELETED, 18, 21) != "None") { #IF ANY COL NAME COMMENTS OR THERE WERE ANY COLUMNS DELETED AT ALL. 
    print(stringr::str_split_fixed(metadata_row$COLUMNS_DELETED, ";", Inf)[1]) #REPORT COLS DELETED SUB-STRING
    if(!is.na(metadata_row$LOOK_COMMENTS)) { print(metadata_row$LOOK_COMMENTS) }
    renamecols_check = readline("Above is info on what columns were deleted,\nas well as on any comments left about column naming by the submitter.\nDo we need to stop and rescue columns?\nPress Y for yes,\nPress any other key to continue.")
    if(renamecols_check == "Y") {
      raw_contents = drive_ls(submitted_raw_id) #GET ALL RAW FILES
      current_raw_id = as_id(raw_contents$id[raw_contents$name == metadata_row$RAW_FILE]) #GET ID OF THE CURRENT RAW FILE
      current_raw_dl = drive_download( #DOWNLOAD THAT FILE LOCALLY
        current_raw_id,
        path = 'temp1.csv', #MIGHT NEED TO BE AN XLS(X) FILE EXTENSION HERE.
        overwrite = TRUE)
      current_raw_df = readxl::read_excel("temp1.csv") #LOAD INTO R.
      which_cols_check = readline("Provide a string of the names of the columns in the RAW file to move to the clean file,\ne.g. 'A, B, C, D', with no quotes and exact spacing.") #GET LIST OF COLS TO PORT
      cols_to_port = str_split_1(which_cols_check, ", ") #SPLIT LIST PROVIDED INTO A VECTOR OF NAMES
      ported_cols = current_raw_df[, cols_to_port] #GRAB COLS BY THOSE NAMES
      print(names(ported_cols))
      new_names_check = readline("Printed above are the names of the columns being ported.\nProvide a string of the new names for these columns,\nas they should appear in the clean file,\ne.g., 'A, B, C, D', with no quotes and exact spacing.\nThese needn't be tidy.") #REQUEST LIST OF NEW COL NAMES
      new_col_names = tidyName(str_split_1(new_names_check, ", ")) #SPLIT THIS LIST INTO A VECTOR
      names(ported_cols) = new_col_names #OVERWRITE THE NAMES OF THE PORTED COLS
      if(nrow(ported_cols) != nrow(current.import)) { #IF RAW AND CLEAN FILES DO NOT MATCH IN LENGTH...
        View(current_raw_df)
        delete_some_rows = readline("It appears that the raw and clean files have differing row numbers.\nPlease enter a string of row numbers in the raw data to delete,\ne.g., '1, 4, 67, 423', with no quotes and exact spacing.") #GET INFO ABOUT WHICH ROWS IN RAW DATA TO DELETE.
        split_rows_nums = as.numeric(str_split_1(delete_some_rows, ", ")) #SPLIT THOSE
        ported_cols = ported_cols[-split_rows_nums, ] #REMOVE THEM BEFORE PORTING.
      }
      if(nrow(ported_cols) != nrow(current.import)) { stop("You messed up! The numbers of rows still don't match!")}
      current.import = cbind(current.import, ported_cols) #CBIND INTO CLEAN FILE
      rewritetoG() #WRITE NEW CLEAN FILE
    }
  }
  #GEN COMMENTS FLAG
  if(!is.na(metadata_row$GEN_COMMS)) { #IF ANY GENERAL COMMENTS...
    print(metadata_row$GEN_COMMS)
    gencomms_check = readline("The submitter left some general comments. Do we need to stop and act?\nPress Y for yes,\nPress any other key to continue.")
    if(gencomms_check == "Y") {
      stop("We need to address some general comments related to the current submission.")
    }
  }

  #TEXT-CONTAINING COLUMNS FLAG 
  #***This check works well and might be preferable, but the summary check later will already produce this same output...
  # if(!is.na(metadata_row$COLUMNS_WTEXT)) {
  #   test_text = current.import %>% 
  #     dplyr::select(-c(SUBMITTER_NAME, SUBMITTER_EMAIL, DOW, SURVEY_START, RAKE_MAX, SUBMIT_TIME, SURVEYORS)) #DON'T LOOK FOR TEXT IN 1ST 8 COLS--METADATA ^^^ WOULD NEED UPDATING IF THIS CHANGES.
  #   test_text = data.frame(sapply(test_text, function(x) {
  #     gsub("\xc2\xa0", "", x, fixed=T) #GET RID OF NON-BREAKING SPACES
  #   }))
  #   test_text[is.na(test_text)] = 0 #TURN NAS TO 0S
  #   test_text = data.frame(convert_column_types(test_text)) #TYPE CONVERT COLS TO NUMERIC IF POSSIBLE
  #   text_columns = sapply(test_text, is.character) #CHECK WHICH ARE STILL CHARACTERS.
  #   names(text_columns) = NULL #ELIM NAMES
  #   
  #   if(any(text_columns == TRUE)) {
  #   
  #   ##THIS WILL LOOP THRU ALL COLUMNS THAT HAVE TEXT, PRINTING FIRST THE NAME OF THE COLUMN, THEN A TABLE OF ALL ITS CONTENT VALUES (WHICH MIGHT BE A LITTLE VERBOSE IF OTHERWISE NUMERIC)
  #   running_logs = rep(FALSE, 8) #ELIM METADATA COLS
  #   for(i in 1:length(text_columns)) { #LOOP OVER TEXTY COLS
  #     running_logs = c(running_logs, text_columns[i])  #TACK ON THE NEXT T/F
  #     if(running_logs[length(running_logs)] == TRUE) { #IF TRUE (TEXTY)
  #        print(names(current.import)[length(running_logs)]) #PRINT NAME OF CURRENT COL (ONLY 1 TRUE POSSIBLE)
  #      print(table(current.import[,length(running_logs)])) #PRINT TABLE OF THIS COL'S VALS
  #     }
  #     running_logs[running_logs==TRUE] = FALSE #COERCE ALL OLD TRUES TO FALSE
  #   }
  #   
  #   textycols_check = readline("The app noted the columns listed above have at least one text-based value in them.\nAll values in each such column were printed above.\nDo we need to stop and make any corrections?\nPress Y for yes,\nPress any other key to continue")
  #   if(textycols_check == "Y") {
  #     stop("Some errant text values may need to be removed from at least one column.")
  #   }
  #   }
  # }
  #CONSECUTIVE SITES FLAG
  if(isTruthy(metadata_row$CONSEC_SITES) &&
     metadata_row$CONSEC_SITES[[1]] == FALSE) {
    absences_check = FALSE
    for(r in 1:nrow(current.import)) { #THE CONCERN IS MISSING ABSENCES, SO CHECK ALL ROWS FOR ANY SIGN OF AN ABSENCE. IF AT LEAST 1 FOUND, PROBABLY NO BIG DEAL...
      if(all(is.na(current.import[r, taxonomic_cols]) |
         current.import[r, taxonomic_cols] == 0)) {
        absences_check = TRUE
       }
      }
        if(absences_check == FALSE) {
        print(sort(current.import$sta_nbr))
        consecsites_check = readline("The app noted that not all station numbers provided were consecutive (or they do not start at 0 or 1),\nsuggesting some absences may have been omitted.\nA check did not reveal any rows lacking taxonomic data, which is suspicious.\nDo we need to stop and ask for clarification?\nPress Y for yes,\nPress any other key to continue.")
        if(consecsites_check == "Y") {
          stop("We should clarify with the submitter if any absences data are missing from the current submission based on sites not being consecutive.")
        }
      }
    }
  #RAKE_UNIT_FLAG--JUST IGNORED AND CHECKED HERE ANEW INSTEAD
  if(any(suppressWarnings(as.numeric(unique(unlist(current.import %>% select(all_of(taxonomic_cols)))))) > current.import$RAKE_MAX[1], na.rm=T)) {
    if(metadata_row$RAKE_UNIT_FLAG[1] != TRUE) { print("The rake max flag isn't working!") }
    print(sort(unique(unlist(current.import %>% select(all_of(taxonomic_cols)))))) 
    print(current.import$RAKE_MAX[1])
    rakeflag_check = readline("1+ rake score value(s) are higher than the max rake score.\nAll values observed in taxonomic columns are listed above, as is the max rake score.\nShould we replace all values exceeding the max rake score?\nPress H for yes,\nPress X to replace the max rake score value instead.\nPress any other key to continue.")
    if(rakeflag_check == "H") {
      newhighrakes = readline("Enter the new value to be given to all values that exceed the max rake score.")
      for(i in taxonomic_cols) {
        notNAs = which(!is.na(current.import[,i])) #Find non-NAs
        toohigh = which(current.import[,i] > current.import$RAKE_MAX[1]) #Find too high values
        both = notNAs[notNAs %in% toohigh] #Find the intersection
        current.import[both, i] = newhighrakes #OVERWRITE IN JUST THOSE SPOTS. 
      }
      rewritetoG()
    }
    if(rakeflag_check == "X") {
      newmaxrake = readline("Enter a new max rake score.")
      current.import$RAKE_MAX = newmaxrake
      rewritetoG()
    }
  }
  #ROWS_CUT_FLAG
  if(!is.na(metadata_row$ROWS_CUT_FLAG) &&
     metadata_row$ROWS_CUT_FLAG == TRUE) { #DID THE USER CUT ANY ROWS MANUALLY USING THE APP?
    rowscut_check = readline("Some rows were cut from the current submission. Do we need to stop and check to see if these rows were cut in error?\nPress Y for yes,\nPress any other key to continue.")
    if(rowscut_check == "Y") {
      print(metadata_row$RAW_FILE)
      stop("We need to consult the raw data file referenced above to see if rows have been cut in error.")
    }
  }
    
    ##BROADER METADATA CHECKS
    
  #DOW, START DATE, AND SUBMIT TIME--DO THEY LOOK OK?
  if(length(unique(current.import$DOW)) > 1 ||
     length(unique(current.import$SURVEY_START)) > 1) {
    print(unique(current.import$DOW))
    print(unique(current.import$SURVEY_START))
    stop("There is more than one DOW or survey start date in the current file--it may need repair.")
  }
  #AUTO-REPLACE IF NOT MATCHING METADATA (SUCH AS WHEN FILES ARE FIXED IN EXCEL AND DATE/TIMES ARE OVERWRITTEN)
  if(current.import$SURVEY_START[1] != 
     as.character(as.Date(metadata_row$SURVEY_DATE[[1]][1]))) { #FIDDLY!
    print(current.import$SURVEY_START[1])
    print(metadata_row$SURVEY_DATE)
    replaceSURVSTART = readline("The survey start date in the current import doesn't match what is reported in the metadata file.\nShould we automatically replace it with the metadata file?\nPress Y for yes.\nPress any other key to continue.")
    if(replaceSURVSTART == "Y") {
      current.import$SURVEY_START = str_sub(metadata_row$SURVEY_DATE[[1]][1], 1, 10)  #This comes in as a list for some unknown reason, so we have to be fussy and also trim it to remove add on nonsense. 
      rewritetoG()
    }
  }
  if(str_sub(current.import$SUBMIT_TIME[1], 1, 16) !=  #THIS WILL GET US CLOSE ENOUGH TO MATCHING PROPER SUBMIT TIMES.
     str_sub(as.character(metadata_row$SUBMIT_DATE[[1]][1]), 1, 16)) {
    print(current.import$SUBMIT_TIME[1])
    print(metadata_row$SUBMIT_DATE)
    replaceSUBTIME = readline("The submit time in the current import doesn't match what is reported in the metadata file.\nShould we automatically replace it with the metadata file?\nPress Y for yes.\nPress any other key to continue.")
    if(replaceSUBTIME == "Y") {
      current.import$SUBMIT_TIME = metadata_row$SUBMIT_DATE
      rewritetoG()
    }
  }
  #VISUAL CHECK STEP
  {print(unique(current.import$DOW))
  print(unique(current.import$SURVEY_START))
  print(unique(current.import$SUBMIT_TIME))
  surveyDOW_check = readline("Press Y if these DOW, start date, and submit time values are valid.\nPress D to change DOW,\nPress S to change survey start (YYYY-MM-DD format!).\nPress T to change submit time.\nEnter multiples to change multiples.\nPress any other key to continue.")}
  if(grepl("D", surveyDOW_check)) {
    newval = readline("Enter the proper DOW value to use. Do not use quotes!")
    current.import$DOW = newval 
    rewritetoG()
  }
  if(grepl("S", surveyDOW_check)) {
    newval = readline("Enter the proper survey start date to use. Do not use quotes! Remember to use YYYY-MM-DD format!")
    current.import$SURVEY_START = newval 
    rewritetoG()
  }
  if(grepl("T", surveyDOW_check)) {
    newval = readline("Enter the proper submit time to use, copied from the metadata file. Do not use quotes!")
    current.import$SUBMIT_TIME = newval 
    rewritetoG()
  }
  #SURVEYORS AND SUBMITTERS--ARE THEY DB-READY IN FORMAT?
   ##HERE, CHECK FOR CERTAIN PUNCTUATION AND IF PRESENT REMOVE
   if(grepl("\\,", current.import$SUBMITTER_NAME[1]) |
      grepl("\\.", current.import$SUBMITTER_NAME[1])) {
     current.import$SUBMITTER_NAME = gsub("\\,", "", current.import$SUBMITTER_NAME)
     current.import$SUBMITTER_NAME = gsub("\\.", "", current.import$SUBMITTER_NAME)
     rewritetoG()
   }
  if(is.null(current.import$SURVEYORS[1])) {
    current.import$SURVEYORS = NA #IF NO SURVEYORS GIVEN, THIS COLUMN GETS CUT SOMEHOW, AND WE CAN REINTRODUCE IT HERE.
  } else {
  if(grepl("\\.", current.import$SURVEYORS[1])) {
    current.import$SURVEYORS = gsub("\\.", "", current.import$SURVEYORS)
    rewritetoG()
   }
  }
  
  print(unique(current.import$SUBMITTER_NAME))
  print(unique(current.import$SURVEYORS))
  if(!current.import$SUBMITTER_NAME[1] %in% unique(current_db$SUBMITTER_NAME)) {
    print("The submitter listed for this survey has not previously appeared in our database, FYI!")
  }
  if(!current.import$SURVEYORS[1] %in% unique(current_db$SURVEYORS)) {
    print("The exact mix of surveyors listed for this survey has not previously appeared in our database, FYI!")
  }
  textcols_check = readline("Do the submitter and surveyor names listed above look ok?\nPress Y if yes.\nPress M to edit the submitter's name.\nPress V to edit the surveyor's names.\nPress multiple letters to edit multiple fields.\nPress N to stop.\nPress any other key to continue.")
      if(grepl("M", textcols_check)) {
        new_submitter_name = readline("Type what the new value should be for the submitter's name (DON'T put quotes!).")
        #OVERWRITE AND RE-WRITE TO GDRIVE
        current.import$SUBMITTER_NAME = new_submitter_name 
        rewritetoG()
      }
      if(grepl("V", textcols_check)) {
        new_surveyors = readline("Type what the new value should be for the surveyors' names (DON'T put quotes!). Also, DON'T USE NA ANYMORE! ANONYMIZE NOW.")
        current.import$SURVEYORS = new_surveyors 
        rewritetoG()
      }
     if(grepl("N", textcols_check)) {
    stop("The check for surveyors and/or submitters failed--these columns may need repair.")
     }

  #LOGIC OF ANY NO_VEG_FOUND COL
  if(any(names(current.import) == "no_veg_found")) {
    rows.marked = which(!current.import$no_veg_found %in% c(NA, 0)) #WHICH ROWS SHOULD HAVE NO TAX DATA?
    rows.unmarked = which(current.import$no_veg_found %in% c(NA, 0)) #WHICH ROWS SHOULD HAVE SOME TAX DATA?

    #CYCLE THRU ROWS, LOOK FOR ERRANT LOGIC
    for(row in rows.marked) {
      if(!all(current.import[row, taxonomic_cols] == 0 |
              is.na(current.import[row, taxonomic_cols]))) {
        print(row)
        print("The row printed above seems to have some taxonomic data even though it is marked as having a non-0, non-NA no_veg_found.")
        print(sort(unique(unlist(current.import[row, taxonomic_cols]))))
        print("Above are all the different values in the taxonomic columns for this row.")
        change_wrk = readline("If you want to change the no veg found value for this row, enter the new value here.\nOtherwise, press N to continue.")
        if(change_wrk != "N") {
          #OVERWRITE AND RE-WRITE TO GDRIVE
          current.import[row, "no_veg_found"] = change_wrk 
          tmp.path = base::file.path(base::paste0(base::tempdir(), "\\"))
          file.tmp = paste0(tmp.path, metadata_row$CLEAN_FILE) 
          write.csv(x = current.import, file = file.tmp, row.names = FALSE) 
          googledrive::drive_upload(media = file.tmp, 
                                    path = submitted_clean_id,
                                    name = metadata_row$CLEAN_FILE)
        }
      }
    }
    for(row in rows.unmarked) {
      if(all(current.import[row, taxonomic_cols] %in% c(NA, 0))) {
        print(row)
        print("The row printed above seems to have no taxonomic data but is not marked as such for no_veg_found.")
        change_wrk = readline("If you want to change the no veg found value for this row, enter the new value here.\nOtherwise, press N to continue.")
        if(change_wrk != "N") {
          current.import[row, "no_veg_found"] = change_wrk 
          #OVERWRITE AND RE-WRITE TO GDRIVE
          tmp.path = base::file.path(base::paste0(base::tempdir(), "\\"))
          file.tmp = paste0(tmp.path, metadata_row$CLEAN_FILE) 
          write.csv(x = current.import, file = file.tmp, row.names = FALSE) 
          googledrive::drive_upload(media = file.tmp, 
                                    path = submitted_clean_id,
                                    name = metadata_row$CLEAN_FILE)
        }
      }
    }
  }
  #LOGIC OF ANY WHOLE_RAKE_DENSITY COL--SIMILAR TO ABOVE
  if(any(names(current.import) == "whole_rake_density")) {
    rows0s = which(current.import$whole_rake_density == 0 |
                     is.na(current.import$whole_rake_density)) #WHICH ROWS SHOULD HAVE NO TAX DATA?
    rowsnon0 = which(current.import$whole_rake_density != 0 &
                      !is.na(current.import$whole_rake_density)) #WHICH ROWS SHOULD HAVE SOME TAX DATA?
    
    #CYCLE THRU ROWS, LOOK FOR ERRANT LOGIC
    for(row in rows0s) {
      if(any(!is.na(current.import[row, taxonomic_cols]) &
             current.import[row,taxonomic_cols] != 0)) {
        print(row)
        print("The row printed above seems to have some taxonomic data even though it is marked as being a 0/NA for whole_rake_density.")
        print(sort(unique(unlist(current.import[row, taxonomic_cols]))))
        print("Above are all the different values in the taxonomic columns for this row.")
        change_wrk = readline("If you want to change the whole rake density value for this row, enter the new value here.\nOtherwise, press N to continue.")
        if(change_wrk != "N") {
          #OVERWRITE AND RE-WRITE TO GDRIVE
          current.import[row, "whole_rake_density"] = change_wrk 
          tmp.path = base::file.path(base::paste0(base::tempdir(), "\\"))
          file.tmp = paste0(tmp.path, metadata_row$CLEAN_FILE) 
          write.csv(x = current.import, file = file.tmp, row.names = FALSE) 
          googledrive::drive_upload(media = file.tmp, 
                                    path = submitted_clean_id,
                                    name = metadata_row$CLEAN_FILE)
        }
      }
    }
    for(row in rowsnon0) {
      if(all(is.na(current.import[row, taxonomic_cols]) |
             current.import[row, taxonomic_cols] == 0)) {
        print(row)
        print("The row printed above seems to have no taxonomic data even though it is marked as having a non-0, non-NA whole_rake_density.")
        change_wrk = readline("If you want to change the whole rake density value for this row, enter the new value here.\nOtherwise, press N to continue.")
        if(change_wrk != "N") {
          current.import[row, "whole_rake_density"] = change_wrk 
          #OVERWRITE AND RE-WRITE TO GDRIVE
          tmp.path = base::file.path(base::paste0(base::tempdir(), "\\"))
          file.tmp = paste0(tmp.path, metadata_row$CLEAN_FILE) 
          write.csv(x = current.import, file = file.tmp, row.names = FALSE) 
          googledrive::drive_upload(media = file.tmp, 
                                    path = submitted_clean_id,
                                    name = metadata_row$CLEAN_FILE)
        }
      }
    }
  }
  #DEPTH COLUMN LOGIC
  if(any(names(current.import) == "depth_ft")) {
    #ANY 0 DEPTHS OR SUPER DEEP DEPTHS?
    current.import$depth_ft = gsub("\xc2\xa0", "", current.import$depth_ft, fixed=T)
    if(any(suppressWarnings(as.numeric(current.import$depth_ft[!is.na(as.numeric(current.import$depth_ft))])) > 30 |
       suppressWarnings(as.numeric(current.import$depth_ft[!is.na(as.numeric(current.import$depth_ft))])) == 0)) {
         odd_depthscheck = readline("There are some really deep depths and/or depths of 0. Should we stop and address these?\nPress Y for yes,\nPress any other key to continue.")
         if(odd_depthscheck == "Y") {
           stop("Stopping to address rows with odd depths values.")
         }
       }
    
    #ROWS WITH TEXTY OR NA VALS IN DEPTH
    textydepths = which(is.na(as.numeric(current.import$depth_ft)))
    nadepths = which(is.na(current.import$depth_ft)) #DON'T INCLUDE THINGS THAT WERE NA TO BEGIN WITH
    textydepths = textydepths[!textydepths %in% nadepths]
    
    #FOR EACH, SEE IF PATTERN OF NON-NUMERIC DEPTH + TAXONOMIC DATA, INDICATING A POTENTIAL NON-THROW UNELIMINATED
    for(r in textydepths) {
      if(any(!is.na(current.import[r, taxonomic_cols]))) {
        print(r)
        textdepth_check = readline("The row printed above appears to have a non-numeric text value in the depth column but taxonomic data otherwise. Is this row ok?\nPress N if no,\nPress any other key to continue.")
        if(textdepth_check == "N") {
          stop("The row printed above has a strange value for depth.")
        }
      }
    }
  }
  
  #WIPE OUT NON-BREAKING SPACES IN THE TAXONOMIC COLS, AS APPLICABLE.
  current.import[, taxonomic_cols] = sapply(current.import[, taxonomic_cols], function(x) {
     gsub("\xc2\xa0", "", x, fixed=T)
  })

  #ALL TAXONOMIC COL VALS--DO THEY LOOK AS WE'D EXPECT?
  print(sort(unique(unlist(current.import[,taxonomic_cols]))))
  taxvals_check = readline("Printed above are all the different non-NA values observed in all the taxonomic columns. Do they all look ok?\nPress N if no,\nPress any other key to continue.")
  if(taxvals_check == "N") {
    stop("At least one value in a taxonomic column looks strange.")
  }
  
  #CHECKING FOR TYPOED COLUMN NAMES
  if(any(!names(current.import) %in% newfieldnames$newfieldname)) { #ANY UNMATCHED NAMES ACCORDING TO OUR DB
    unmatched_names = names(current.import)[which(!names(current.import) %in% newfieldnames$newfieldname)] #PULL THOSE OUT
    unmatched_names = unmatched_names[!unmatched_names %in% c("SUBMITTER_NAME", "SUBMITTER_EMAIL", "DOW", "SURVEY_START", "RAKE_MAX", "SUBMIT_TIME", "SURVEYORS")] #REMOVE THE METADATA ^^^WOULD NEED TO BE UPDATED IF OTHER METADATA COLS GET ADDED...
    for(name in unmatched_names) { #FOR EACH NONMATCH
      print(name) #ASK ME TO RENAME OR DELETE IT.
      colfix_check = readline("This column name is in this file, but it isn't in our lookup table. What should it be replaced with?\nType a replacement column name or else type 'D' to delete this column.")
      if(colfix_check != "D") {
        names(current.import)[names(current.import) == name] = colfix_check
        rewritetoG()
      } else {
        current.import = current.import %>% dplyr::select(-{{name}})
        rewritetoG()
        print("That column was deleted.")
      }
    }
  }
  
  #TRY SMART COLUMN CONVERSION TO SEE IF WE CAN GET NUMERALS FOR THE TAXONOMIC COLS AT LEAST
  summary_check_df = current.import
  summary_check_df = data.frame(convert_column_types_approv(summary_check_df))
  
  #SUMMARY OF ALL VALS FOR ONE MORE GOOD LOOK (FIRST NUMERICS, THEN CATEGORICALS, NO METADATA COLS)
  print(summary(summary_check_df)[-c(2, 4, 5), unlist(lapply(summary_check_df, is.numeric))][,-c(1:2)])
  print(summary(summary_check_df)[, unlist(lapply(summary_check_df, is.factor))][,-c(1:6)])
  summary_check = readline("Does the summary above look ok?\nPress N if no,\nPress any other key to continue.\nYou can delete specific rows next, if needed.")
  if(summary_check == "N") {
    stop("The summary check of the data for this import failed--the file may need repair.")
  }
  
  ##FEATURE ENABLING THE DELETING OF SPECIFIC ROWS.
  delete_rowscheck = readline("Should we delete any rows before proceeding?\nPress Y for yes.\nPress any other key to continue.")
  if(delete_rowscheck == "Y") {
    delete_theserows = readline("Enter row number(s) of the rows to delete. Separate multiple row numbers with a comma and no spaces.")
    delete_row_nums = str_split_1(delete_theserows, ",")
    current.import = current.import[-as.numeric(delete_row_nums), ]
    rewritetoG()
  }
  
  ##ASK IF I WANT TO DELETE ANY COLUMNS
  delete_colscheck = readline("Want to delete any columns?\nPress Y for yes.\nPress any other key to continue.")
  if(delete_colscheck == "Y") {
    delete_thesecols = readline("Enter column names to delete. Make these one string, separated by a comma, and no spaces. Do not use quotes.")
    delete_thesecols = str_split_1(delete_thesecols, ",")
    for(i in 1:length(delete_thesecols)) {
    current.import = current.import %>% 
      dplyr::select(-!!sym(delete_thesecols[i]))
    }
    rewritetoG()
  }
  
  #RELOCATE SURVEYORS COLUMN IF IT DIDN'T PREVIOUSLY EXIST (COMES IN AS NA)
  if(names(current.import)[8] != "SURVEYORS") {
    current.import = current.import %>% 
      select(SUBMITTER_NAME, SUBMITTER_EMAIL, DOW, SURVEY_START, 
             RAKE_MAX, SUBMIT_TIME, SURVEYORS, everything())
    print("We're shifting the metadata columns to where they belong.")
    rewritetoG()
  }

  #INSERT A STOP CHECK TO MAKE SURE THAT LOCATION DATA SEEMED TO CONVERT PROPERLY.
  if(metadata_row$SPATIAL_DAT == "Yes, this file contains UTM data for every point.") {
    utm_datcheck = readline("The user said they provided ONLY UTM location data.\nThese should have been transitioned to lat/long data by the app.\nDoes it look like this happened properly?\nPress N to stop if not.")
    if(utm_datcheck == "N") { stop("Conversion of UTM data to lat/long data did not occur properly.") }
  }
  
  ##USING THE LOC_DAT ANSWER TO HELP DECIDE WHERE THIS FILE SHOULD GO--GOOD GUT CHECK FOR IF THIS FILE IS TRULY READY
  print(metadata_row$SPATIAL_DAT)
  wherego_check = readline("The above was the submitter's answer to the location_data question. Based on this answer, should this file go into:\n'need location data joined (PRESS J)',\n'need alt location data (PRESS A)',\nor 'compiled (PRESS C)?")

  #IF THE FILE PASSES, WRITE IT INTO THE DATABASE
  if(any(list.files("upstream/") %in% "db_unified.parquet")) {
    db.new = read_parquet("upstream/db_unified.parquet")
    grow.dat = rbindlist(list(current.import, db.new), fill = T)
    write_parquet(as.data.frame(grow.dat), sink = "upstream/db_unified.parquet")

    print(metadata_row$CLEAN_FILE)
    pausetomove = readline("The file noted above is ready to be moved into the approved and compiled subfolder indicated by the letter initial printed above.\nDo that, then press any key to continue.\nNow's a good time to confirm that grow.dat is longer than whatever it was!")
  } else {
    stop("Where'd the database file go??")
  }
}

