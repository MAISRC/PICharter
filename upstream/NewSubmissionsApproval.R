#THIS FILE WALKS THRU THE SUBMISSION QA/QC PROCESS USING A BUNCH OF CHECKSUMS AND FLAGS. IN MANY INSTANCES, THE FILE CAN BE REPAIRED AND RESAVED AUTOMATICALLY. MORE COMPLEX FIXES WILL TRIGGER STOPS. AT THE END, THE FILE WRITES THE RESULT INTO THE DATABASE FILE.

#NOTE: THE CREATINGLAKESUMMARIES.R FILE WOULD STILL NEED TO BE RUN AFTER APPROVING A NEW SUBMISSION TO MAKE THE APP FULLY UP TO DATE!

#Source the following file once per approval round!
#source("upstream/NewApprovalsPreLoad.R")

# Load, process, QA/QC, and compile approved submissions --------

#FIND ALL AVAILABLE NEW SUBMISSIONS, LIST, PREPARE TO LOOP OVER.
all_subs = drive_ls(submitted_clean_id)
content.ids = all_subs$id
content.names = all_subs$name
content.ids = content.ids[!as.character(content.ids) %in%  #IGNORE IRRELEVANT CONTENTS OF THIS FOLDER
                            as.character(c(already_approved_id))]
content.names = content.names[!as.character(content.names) %in%
                            as.character(c(already_approved_name))]

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
  metadata_row$SURVEY_DATE = format(metadata_row$SURVEY_DATE, "%Y-%m-%d") #<--NEW UPDATE IS MAKING THIS GET READ AS A POSITCX INSTEAD.
  
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
  currentsubbasin = current.import$subbasin[1] #GET SUBBASIN
  
  #NEW--MUST BRANCHING LOGIC FOR THE CHECK HERE IF THIS IS A SUBBASIN
  if(is.null(currentsubbasin)) {
    next_check = length(which(current_db$DOW == currentdow &  #LOOK FOR THESE SAME VALS IN THE DB
                   current_db$SURVEY_START == currentsurveydate)) > 0
  } else {
    next_check = length(which(current_db$DOW == currentdow &  #LOOK FOR THESE SAME VALS IN THE DB
                                current_db$SURVEY_START == currentsurveydate &
                              current_db$subbasin == currentsubbasin)) > 0
  }

  if(next_check){
    stop("There's evidence that this is a duplicate survey record. Please verify before continuing submission.")
  } else {
    if(is.null(currentsubbasin)) {
    if(isTruthy(grow.dat) &&
       length(which(grow.dat$DOW == currentdow &
                    grow.dat$SURVEY_START == currentsurveydate)) > 0) {
      stop("There's evidence that this is a duplicate survey record (within the current submission). Please verify before continuing submission.")
    }
    } else {
      if(isTruthy(grow.dat) &&
         length(which(grow.dat$DOW == currentdow &
                      grow.dat$SURVEY_START == currentsurveydate &
                      grow.dat$subbasin == currentsubbasin)) > 0) {
        stop("There's evidence that this is a duplicate survey record (within the current submission). Please verify before continuing submission.")
      }
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
        path = metadata_row$RAW_FILE, #MAINTAIN NAME SAVED BY APP PLUS FILE EXTENSION.
        overwrite = TRUE)
      if(grepl(".xls", metadata_row$RAW_FILE)) { #LOAD INTO R, BASED ON FILE TYPE
      current_raw_df = readxl::read_excel(metadata_row$RAW_FILE)
      } else {
      current_raw_df = read.csv(metadata_row$RAW_FILE) 
      print("Here are the column names in the raw data file, in case they don't match what the app reported due to formatting differences: ")
      print(names(current_raw_df))
      }
      which_cols_check = readline("Provide a string of the names of the columns in the RAW file to move to the clean file,\ne.g. 'A, B, C, D', with no quotes and exact spacing.") #GET LIST OF COLS TO PORT
      cols_to_port = str_split_1(which_cols_check, ", ") #SPLIT LIST PROVIDED INTO A VECTOR OF NAMES
      ported_cols = dplyr::select(current_raw_df, dplyr::all_of(cols_to_port)) #GRAB COLS BY THOSE NAMES
      print(names(ported_cols))
      new_names_check = readline("Printed above are the names of the columns being ported.\nProvide a string of the new names for these columns,\nas they should appear in the clean file,\ne.g., 'A, B, C, D', with no quotes and exact spacing.\nThese needn't be tidy.") #REQUEST LIST OF NEW COL NAMES
      new_col_names = tidyName(str_split_1(new_names_check, ", ")) #SPLIT THIS LIST INTO A VECTOR
      names(ported_cols) = new_col_names #OVERWRITE THE NAMES OF THE PORTED COLS

      if(nrow(ported_cols) != nrow(current.import)) { #IF RAW AND CLEAN FILES DO NOT MATCH IN LENGTH...
        View(current_raw_df)
        delete_some_rows = readline("It appears that the raw and clean files have differing row numbers.\nPlease enter a string of row numbers in the raw data to delete,\ne.g., '1, 4, 67, 423', with no quotes and exact spacing.") #GET INFO ABOUT WHICH ROWS IN RAW DATA TO DELETE.
        split_rows_nums = as.numeric(str_split_1(delete_some_rows, ", ")) #SPLIT THOSE
        ported_cols = ported_cols[-split_rows_nums, , drop = F] #REMOVE THEM BEFORE PORTING.
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
    cat(
      "1+ rake score value(s) exceed the max rake score.\n",
      "Observed values and the max are printed above.\n",
      "H = Replace high values\n",
      "X = Replace the max rake score\n"
    )
    rakeflag_check = readline("Continue? (H/X/Enter): ")
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
  
  #HERE, WE CHECK TO SEE IF THE DOW IS 7 CHARACTERS AND IF SO COERCE IT TO 8 CHARACTERS AUTOMATICALLY.
  if(nchar(current.import$DOW[1]) == 7) {
    current.import$DOW = paste0("0", as.character(current.import$DOW))
    print("The DOW wasn't 8 characters--appending leading 0.")
    rewritetoG()
  }
  
  #THEN, WE CHECK TO SEE IF THE LAST 2 DIGITS ARE NOT 00 AND COERCE THEM TO 00 IF SO--DEPRECATED
  if(str_sub(current.import$DOW[1], 7, 8) != "00") {
    
    print("Hey Alex, a subbasin DOW has slipped through somehow! Fix your Submissions code!")
    
    # subbasin_digits = str_sub(current.import$DOW[1], 7, 8)
    # current.import$subbasin = subbasin_digits #STASH THIS INFO IN THE SUBBASIN COLUMN FOR FUTURE REFERENCE.
    # 
    # str_sub(current.import$DOW[1], 7, 8) = "00" #SET THE LAST TWO DIGITS TO 00 FOR THE FIRST ENTRY
    # current.import$DOW = current.import$DOW[1] #THEN, EXTRAPOLATE THAT TO ALL ENTRIES.
    # print("The DOW was for a sub-basin of a lake--coercing last two digits of DOW to 00.")
    # rewritetoG()
  }
  
  
  #VISUAL CHECK STEP
  {print(unique(current.import$DOW))
  print(unique(current.import$SURVEY_START))
  print(unique(current.import$SUBMIT_TIME))
  surveyDOW_check = readline("Press Y if these DOW, start date, and submit time values are valid.\nPress D to change DOW,\nPress S to change survey start (YYYY-MM-DD format!).\nPress T to change submit time.\nEnter multiples to change multiples.\nPress any other key to continue.")}
  if(grepl("D", surveyDOW_check)) {
    newval = readline("Enter the proper DOW value to use. Do not use quotes!")
    if(nchar(newval) == 8 &
       str_sub(newval, 7, 8) == "00") { #THIS IS PAST THE SUBBASIN CHECK, SO IF THERE'S A SUBBASIN CODE HERE, WE NEED TO EXTRACT IT.
    current.import$DOW = newval 
    } else {
    current.import$DOW = paste0(str_sub(newval, 1, 6), "00")
    current.import$subbasin = str_sub(newval, 7, 8)
    }
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
  #HOPEFULLY MORE ROBUST NOW TO AVOID OVER-TRIGGERS
  if (isTRUE(grepl(",", current.import$SUBMITTER_NAME[1], fixed = TRUE)) ||
      isTRUE(grepl(".", current.import$SUBMITTER_NAME[1], fixed = TRUE))) {
     current.import$SUBMITTER_NAME = gsub("\\,", "", current.import$SUBMITTER_NAME)
     current.import$SUBMITTER_NAME = gsub("\\.", "", current.import$SUBMITTER_NAME)
     rewritetoG()
   }
  #THIS CHECK IS NOW MORE ROBUST TO HOPEFULLY NOT TRIGGER ERRONEOUSLY
  if(!"SURVEYORS" %in% names(current.import) ||
     all(is.na(current.import$SURVEYORS)) || current.import$SURVEYORS[1] == "") {
    current.import$SURVEYORS = "Unnamed hardworking surveyor(s)" #IF NO SURVEYORS GIVEN, THIS COLUMN GETS CUT SOMEHOW, AND WE CAN REINTRODUCE IT HERE.
  } else {
  if(grepl("\\.", current.import$SURVEYORS[1])) {
    current.import$SURVEYORS = gsub("\\.", "", current.import$SURVEYORS)
    rewritetoG()
   }
  }
  #HERE, CHECK FOR NA IN SURVEYOR STRING AND THEN REPLACE WITH ANONYMIZED STRING--DEPRECATED
  if(grepl("NA", current.import$SURVEYORS[1])) {
    
    print("Hey Alex, there is something that looks like an anonymous surveyor in here! Fix your submission file!")
    
    # current.import$SURVEYORS = gsub("NA", "Unnamed hardworking surveyor(s)", current.import$SURVEYORS)
    # print("At least one substring in SURVEYORS was NA, so this was overwritten with the anonymized surveyor string.")
    # rewritetoG()
  }
  
  print(unique(current.import$SUBMITTER_NAME))
  print(unique(current.import$SURVEYORS))
  if(!current.import$SUBMITTER_NAME[1] %in% unique(current_db$SUBMITTER_NAME)) {
    print("The submitter listed for this survey has not previously appeared in our database, FYI!")
  }
  
  #HERE WE WANT TO CHECK AND FLAG IF THIS SET OF SURVEYORS CONTAINS ANY NEW NAMES
  old_names = unique(trimws(unlist(strsplit(unique(current_db$SURVEYORS), ","))))
  new_names = trimws(unlist(strsplit(current.import$SURVEYORS[1], ",")))
  
  new_only = setdiff(new_names, old_names)
  new_only = new_only[!is.na(new_only) & new_only != "NA" & new_only != ""]
  
  if (length(new_only) > 0) {
    print(paste0("These appear to be new surveyor name(s) to our database: ", paste(new_only, collapse = ", ")))
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


          #OVERWRITE AND RE-WRITE TO GDRIVE
        print(paste0("The no_veg_found value for row ", row, "was not 0/NA but there were positive rake scores there--replacing with 0."))
        if(row == rows.marked[1]) {
          heads_up = readline("Press any key to continue")
        }
          current.import[row, "no_veg_found"] = 0 
          rewritetoG()
        }

    }
    
    for(row in rows.unmarked) {
      if(all(current.import[row, taxonomic_cols] %in% c(NA, 0))) {

        print(paste0("The no_veg_found value for row ", row, "was 0/NA but there were no positive rake scores there--replacing with 1."))
        if(row == rows.unmarked[1]) {
          heads_up = readline("Press any key to continue")
        }
          current.import[row, "no_veg_found"] = 1 
          rewritetoG()

      }
    }
  }
  
  #LOGIC OF ANY WHOLE_RAKE_DENSITY COL--SIMILAR TO ABOVE--DEPRECATED
#   if(any(names(current.import) == "whole_rake_density")) {
#     rows0s = which(current.import$whole_rake_density == 0 |
#                      is.na(current.import$whole_rake_density)) #WHICH ROWS SHOULD HAVE NO TAX DATA?
#     rowsnon0 = which(current.import$whole_rake_density != 0 &
#                       !is.na(current.import$whole_rake_density)) #WHICH ROWS SHOULD HAVE SOME TAX DATA?
#     
#     #CYCLE THRU ROWS, LOOK FOR ERRANT LOGIC
#     for(row in rows0s) {
#       if(any(!is.na(current.import[row, taxonomic_cols]) &
#              current.import[row,taxonomic_cols] != 0)) {
# 
#         print("Hey Alex, some WRD non-logic is slipping through! Correct your submissions process! (#1)")
#         
#         # possible_vals = sort(unique(as.numeric(unlist(current.import[row, taxonomic_cols]))))
#         # max_val = max(possible_vals, na.rm=T)
#         # 
#         # if(length(max_val) == 1) {
#         #   #OVERWRITE AND RE-WRITE TO GDRIVE
#         #   print(paste0("The whole_rake_density value for row ", row, " was 0/NA but there were positive rake scores--replacing with the highest one."))
#         #   if(row == rows0s[1]) {
#         #     heads_up = readline("Press any key to continue")
#         #   }
#         #   current.import[row, "whole_rake_density"] = max_val 
#         #   rewritetoG()
#         # }
#       }
#     }
#     
#     for(row in rowsnon0) {
#       #REPLACING NON-0S WITH 0S...
#       if(all(is.na(current.import[row, taxonomic_cols]) |
#              current.import[row, taxonomic_cols] == 0)) {
# 
#         print("Hey Alex, some WRD non-logic is slipping through! Correct your submissions process! (#2)")
# # 
# #           print(paste0("The whole_rake_density value for row ", row, "was not 0/NA but there weren't positive rake scores--replacing with 0."))
# #         current.import[row, "whole_rake_density"] = 0
# #           rewritetoG()
# 
#       }
#       #REPLACING WRD VALUES WITH TAXONOMIC VALUES THAT WERE HIGHER.
#       if(any(suppressWarnings(as.numeric(current.import[row, taxonomic_cols])) > 
#              current.import$whole_rake_density[row], 
#              na.rm = T)) {
#         
#         print("Hey Alex, some WRD non-logic is slipping through! Correct your submissions process! (#3)")
#         
#         # print(paste0("The whole_rake_density value for row ", row, " was ", current.import$whole_rake_density[row], ", but there were taxonomic rake score values higher than that--replacing with the highest one."))
#         # current.import$whole_rake_density[row] = max(suppressWarnings(as.numeric(current.import[row, taxonomic_cols])), na.rm = T)
#         # rewritetoG()
#       }
#       # #REPLACING HIGHER WRD VALUES WITH A MAX TAXONOMIC VALUE THAT WAS LOWER --UNDESIRABLE! A 4 FOR WRD MIGHT BE A BUNCH OF 1S SUMMED UP, EFFECTIVELY. 
#       # if(current.import$whole_rake_density[row] > 
#       #        max(suppressWarnings(as.numeric(current.import[row, taxonomic_cols])), na.rm = T)) {
#       #   print(paste0("The whole_rake_density value for row ", row, " was ", current.import$whole_rake_density[row], ", but there were no taxonomic rake score values that high--replacing with the max rake score observed."))
#       #   current.import$whole_rake_density[row] = max(suppressWarnings(as.numeric(current.import[row, taxonomic_cols])), na.rm = T)
#       #   rewritetoG()
#       # }
#       
#     }
#   }
  
  
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
  taxvals_check = readline("Printed above are the different non-NA values in all taxonomic columns. 
  Do they all look ok? Press N if no; Press D to divide columns by 100.
  Press any other key to continue.")
  if(taxvals_check == "N") {
    stop("At least one value in a taxonomic column looks strange.")
  }
  
  if(taxvals_check == "D") {
    divide_cols = readline("Which column numbers should be divided by 100?
                           Provide a min,max in that format.")
    divide_cols2 = str_split_1(divide_cols, pattern = ",")
    if(length(divide_cols2) != 2) { stop("You done goofed!") }
    #THIS WILL OVERRIDE ANY TEXT VALS!
    print("Dividing those columns by 100...")
    current.import[divide_cols2[1]:divide_cols2[2]] = 
      sapply(current.import[divide_cols2[1]:divide_cols2[2]], 
             function(x) {as.numeric(x)/100})
    rewritetoG()
  }
  
  
  #CHECKING FOR TYPOED COLUMN NAMES
  if(any(!names(current.import) %in% newfieldnames$newfieldname)) { #ANY UNMATCHED NAMES ACCORDING TO OUR DB
    unmatched_names = names(current.import)[which(!names(current.import) %in% newfieldnames$newfieldname)] #PULL THOSE OUT
    unmatched_names = unmatched_names[!unmatched_names %in% c("SUBMITTER_NAME", "SUBMITTER_EMAIL", "DOW", "SURVEY_START", "RAKE_MAX", "SUBMIT_TIME", "SURVEYORS")] #REMOVE THE METADATA ^^^WOULD NEED TO BE UPDATED IF OTHER METADATA COLS GET ADDED...
    for(name in unmatched_names) { #FOR EACH NONMATCH
      print(name) #ASK ME TO RENAME OR DELETE IT.
      colfix_check = readline("This column name is in this file, but it isn't in our lookup table. What should it be replaced with?\nType a replacement column name (or the same name again) or else type 'D' to delete this column.")
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
  
  
  #CHECK FOR MALFORMED LOCATION DATA (LIKELY UTM THAT NEED TO BE CONVERTED.)
  if(all(c("latitude", "longitude") %in% names(current.import)) &&
     any(current.import$latitude < 43,
         current.import$latitude > 49,
         current.import$longitude > -89,
         current.import$longitude < -97,
         na.rm = T)) {
    
    weird_locdata = readline("It looks like this file may contain invalid lat/long data.
                           What should we do? Press D to delete them. Press C to 
                           convert them to lat/long from UTM. Press any other key
                           to skip.")
    
    if(weird_locdata == "D") {
      current.import$latitude = NULL
      current.import$longitude = NULL
      print("Deleting those columns...")
      rewritetoG()
    }
    
    if(weird_locdata == "C") {
      
      give_me_col_names = readline("Ok. What column names are we looking for? 
                                   Use format \"nameLat, nameLong\" exactly!")
      
      both_col_names = str_split_1(give_me_col_names, pattern = ", ")
      print("Converting both those columns from UTM to lat/long...")
      
      current.import = convertUTMtoLatLong(current.import, both_col_names[1], both_col_names[2])
      rewritetoG()
    }
    
  }
  
  
  #TRY SMART COLUMN CONVERSION TO SEE IF WE CAN GET NUMERALS FOR THE TAXONOMIC COLS AT LEAST
  summary_check_df = current.import %>% dplyr::select(-c(SUBMITTER_NAME, SUBMITTER_EMAIL, DOW, SURVEY_START, RAKE_MAX, SUBMIT_TIME, SURVEYORS))
  summary_check_df = data.frame(convert_column_types_approv(summary_check_df))
  
  #SUMMARY OF ALL VALS FOR ONE MORE GOOD LOOK (FIRST NUMERICS, THEN CATEGORICALS, NO METADATA COLS)
  num_cols = vapply(summary_check_df, is.numeric, logical(1))
  fac_cols = vapply(summary_check_df, is.factor,  logical(1))
  
  #PRINT NUMERICAL SUMMARY STATS
  if (any(num_cols)) {
    num_sum = summary(summary_check_df[, num_cols, drop = FALSE])
    keep = setdiff(seq_len(nrow(num_sum)), c(2,4,5))
    print(num_sum[keep, , drop = FALSE])
  }
  
  #PRINT CATEGORICAL SUMMARY STATS
  if (any(fac_cols)) {
    print(summary(summary_check_df[, fac_cols, drop = FALSE]))
  }
  
  summary_check = readline(
    "Does the summary above look ok?\nPress N if no,\nPress any other key to continue.\nYou can delete specific rows next, if needed."
  )
  if (summary_check == "N") {
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
  wherego_check = readline("The above was the submitter's answer to the location_data question. Based on this answer, how would you characterize this submission:\nIt needs location data joined (PRESS J),\nIt needs to be returned to the submitter due to an issue (PRESS A),\nor it's ready to be archived (PRESS C)?")

  #IF THE FILE PASSES, WRITE IT INTO THE DATABASE
  if(any(list.files("upstream/") %in% "db_unified.parquet")) {
    db.new = read_parquet("upstream/db_unified.parquet")
    grow.dat = rbindlist(list(current.import, db.new), fill = T)
    
    if(nrow(grow.dat) == nrow(db.new)) { stop("Why does it seem like the database file didn't just grow in length?") }
    
    write_parquet(as.data.frame(grow.dat), sink = "upstream/db_unified.parquet")

    print(metadata_row$CLEAN_FILE)
    pausetomove = readline("The file noted above is ready to be moved into the approved and compiled subfolder indicated by the letter initial printed above.\nDo that, then press any key to continue.")
  } else {
    stop("Where'd the database file go??")
  }
}

