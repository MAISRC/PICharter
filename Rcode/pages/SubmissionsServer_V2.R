submissionsServer_V2 = function(input, output, session) {


# SECTION 1 -- REACTIVE VALUES --------------------------------------------

  sub_reactives = reactiveValues(
    isNextOpen = FALSE, #IS NEXT BUTTON ENABLED?
    sub_complete = FALSE, #IS CURRENT SUBMISSION COMPLETE?
    current_sub = NULL, #IS THERE A SUBMITTED DATA FILE YET?
    current_path = NULL, #PATH TO SUBMITTED FILE
    max_start_row = Inf, #MAX PLAUSIBLE STARTING ROW--GETS OVERRIDDEN DYNAMICALLY.
    max_end_row = Inf, #MAX PLAUSIBLE ENDING ROW--GETS OVERRIDDEN DYNAMICALLY.
    lake_name_delay = Sys.time(), #WHEN DID THE USER LAST TYPE IN THE LAKE NAME SEARCH BOX?
    COLUMNS_DELETE = NULL, #WHAT COLUMNS WERE DELETED DUE TO UNRECOGNIZED NAMES?
    COLUMNS_EMPTY = NULL, #WHAT COLUMNS WERE DELETED DUE TO BEING EMPTY?
    delete_message = NULL, #MESSAGE TO REPORT TO USERS ABOUT UNRECOGNIZED/AMBIGUOUS COLUMN NAMES.
    delete_cols = NULL, #OLD NAMES OF COLUMNS MARKED 'DELETE.'
    ambiguous_cols = NULL, #OLD NAMES OF COLUMNS MARKED 'AMBIGUOUS.'
    start_rows_cut_flag = FALSE, #WERE ANY ROWS CUT AT THE START OF THE TABLE?
    end_rows_cut_flag = FALSE, #WERE ANY ROWS CUT AT THE END OF THE TABLE?
    sub_dfV1 = data.frame(matrix(NA, nrow=1)), #STARTING DATA FRAME
    sub_dfV2 = data.frame(matrix(NA, nrow=1)), #DATA FRAME POST START-ROW CLIPPING
    sub_dfV3 = data.frame(matrix(NA, nrow=1)), #DATA FRAME POST END-ROW CLIPPING
    sub_dfV4 = data.frame(matrix(NA, nrow=1)), #DATA FRAME POST FLIPPING
    sub_dfV5 = data.frame(matrix(NA, nrow=1)), #DATA FRAME POST NON-THROWS CLIPPING
    sub_dfV6 = data.frame(matrix(NA, nrow=1)), #DATA FRAME POST RENAMING
    dupe_check = FALSE, #DO WE HAVE A POTENTIAL DUPE SURVEY?
    rattle_the_cage = 0 #BY DISABLING THE NEXT BUTTON AUTOMATICALLY ON EVERY TRANSITION, YOU HAVE HAVE SITUATIONS WHERE isNextOpen DOESN'T CHANGE BUT WE'D STILL WANT TO ASSESS IF THE NEXT BUTTON SHOULD BE OPEN ON A TRANSITION. BY RANDOMLY CHANGING THIS VALUE, WE CAN FORCE THAT ASSESSMENT TO OCCUR. 
  )
  
  search_input <- debounce(reactive({ input$lake_search }), 500) #PREVENT INVALIDATION IN THE LAKE SEARCH BAR UNTIL INACTIVE FOR X PERIOD OF TIME.
  
  next_button_debounce <- debounce(reactive({ input$new_sub_next }), 300) #PREVENT INVALIDATION IN THE NEXT BUTTON BEYOND A RATE OF 1 CLICK PER 1/3 SECONDS. 


# SECTION 2--CONSTANT STATUS MAINTAINERS ----------------------------------

  #OBSERVER WATCHING ACTIVE TAB--COLORS TABS FOR PROGRESS
  observe({
    
    active_tab <- as.numeric(input$submission_tabset) # FIND CURRENT TAB
    
    #SEND JQ TAB # TO JS INFRASTRUCTURE FOR COLORING. SEE GLOBAL UI FOR DETAILS.
    session$sendCustomMessage(type = 'colorTabs', 
                                message = active_tab)
    
    output$progress_bar_text = renderUI({
      
      p(paste0("You're on question #", active_tab, " of 19."),
        `aria-live` = "polite",
        `aria-atomic` = "true")
      
    })
    
      
  })
  
  #OBSERVER WATCHING INPUTS, DOING VALIDATION, TO AFFECT NEXT BUTTON STATUS. 
  observe({

    theTab = as.numeric(input$submission_tabset) #CURRENT TAB
    
    #ESTABLISH INPUT TO VALIDATE
    if(theTab == 1) { theInput = input$acknowledge_data2 }
    if(theTab == 2) { theInput = input$submitter_name2 }
    if(theTab == 3) { theInput = input$submitter_email2 }
    if(theTab == 4) { theInput = input$surveyors_names2 }
    if(theTab == 5) { theInput = input$lake_name2 }
    if(theTab == 6) { theInput = input$survey_date2 }
    if(theTab == 7) { theInput = input$depth_units2 }
    if(theTab == 8) { theInput = input$rake_units2 }
    if(theTab == 9) { theInput = input$data_file2 }
    if(theTab == 10) { theInput = input$start_row2 }
    if(theTab == 11) { theInput = input$end_row2 }
    if(theTab == 12) { theInput = input$flip_table2 }
    if(theTab == 13) { theInput = input$non_throws2 }
    if(theTab == 14) { theInput = input$non_throws_val2 }
    if(theTab == 15) { theInput = input$rename_cols2 }
    if(theTab == 16) { theInput = input$rename_comms2 }
    if(theTab == 17) { theInput = input$location_data2 }
    if(theTab == 18) { theInput = input$location_file2 }
    if(theTab == 19) { theInput = input$gen_comms2 }

    #VALIDATE AND SET NEXT BUTTON STATUS
    sub_reactives$isNextOpen = subValidate(theTab, theInput)
    
    sub_reactives$rattle_the_cage = rnorm(1, 0, 1)

  })
  
  #OBSERVER PROVIDING ADDITIONAL VALIDATION LAYERS
  observe({
   
    #LOCK NEXT IF START ROW > MAX START ROW
    if(isTruthy(input$start_row2) && 
        input$start_row2 > sub_reactives$max_start_row) {
      
      sub_reactives$isNextOpen = FALSE
    }
    
    #LOCK NEXT IF END ROW > MAX END ROW OR < 10
    if(isTruthy(input$end_row2) &&
       isTruthy(input$start_row2) &&
       (input$end_row2 > sub_reactives$max_end_row |
       input$end_row2 < 10)) {
      
      sub_reactives$isNextOpen = FALSE
    }
    
    #LOCK NEXT IF LOCATION FILE == ZIP FILE BUT DOESN'T CONTAIN A SHP FILE
    if(isTruthy(input$location_file2) &&
       input$location_data2 == "No, my data file doesn't contain location data, but I can upload an additional file containing those location data." &&
       grepl(".zip", input$location_file2$datapath)) {
      
      tmp.path_zip = file.path(paste0(tempdir(), "\\")) #MAKE TEMP DIR. CAN'T PRE-RENDER...NEEDS TO BE FRESH EVERY TIME.
      utils::unzip(input$location_file2$datapath, exdir = tmp.path_zip) #UNZIP INTO
      
      if(!isTruthy(grepl(".shp", list.files(tmp.path_zip)))) {
        
        sub_reactives$isNextOpen = FALSE
      }
    }

  })

  
  #OBSERVER TRIGGERING VALIDATION WARNING MESSAGES TO USERS.
  
observe({

  #DISPLAY WARNING IF USER IS CONFUSED
  if(isTruthy(input$acknowledge_data2) &&
     input$acknowledge_data2 == "No, I'm confused by this.") {
    
    shinyjs::show("confused_warning")
    output$confused_warning = renderText({
      HTML("<strong>Note: That\'s ok, this is a subtle distinction! That said, it\'s also an important one--please email Alex at MAISRC (bajcz003@umn.edu) and he'd be happy to work through this issue with you before you submit.</strong>")
    })
    
  } else {
    
    #DISPLAY WARNING IF USER HAS NOT PICKED ANYTHING YET.
    if(isTruthy(input$acknowledge_data2) &&
          input$acknowledge_data2 == "No selection") {
      
      shinyjs::show("confused_warning")
      output$confused_warning = renderText({
        HTML("<strong>Important: Please make a selection. This is a required question; the \'Next\' button below this question will remain locked until you have made a choice.</strong>")
      })
      
    } else {
    shinyjs::hide("confused_warning")
    output$confused_warning = renderText({})
    }
  }
  
  
  #DISPLAY WARNING IF USER DOESN'T ENTER VALID NAME
  if(isTruthy(input$submitter_name2) &&
     sub_reactives$isNextOpen == FALSE) {
    
    output$name_val_warning = renderText({
      HTML("<strong>Important: Please enter a full person or organization name (don\'t use abbreviations or numbers).</strong>")
    })
  } else {
    
    #IF THE USER HASN'T PUT ANYTHING, RENDER THE WARNING.
    if(input$submitter_name2 == "") {
      
    output$name_val_warning = renderText({
        HTML("<strong>Important: Please enter your name. This is a required question; the \'Next\' button below this question will remain locked until you have entered something at least 8 characters in length.</strong>")
      })
      
    } else {
    
    output$name_val_warning = renderText({ })
    }
  }
  
  
  #DISPLAY WARNING IF USER DOESN'T ENTER VALID EMAIL ADDRESS
  if(isTruthy(input$submitter_email2) &&
     sub_reactives$isNextOpen == FALSE) {
    
    output$email_val_warning = renderText({
      HTML("<strong>Important: Please enter a full email address in text@text.text format.</strong>")
    })
  } else {
    
    #IF THE USER HASN'T PUT ANYTHING, RENDER THE WARNING.
    if(input$submitter_email2 == "") {
      
      output$email_val_warning = renderText({
        HTML("<strong>Important: Please enter your email address. This is a required question; the \'Next\' button below this question will remain locked until you have entered something formatted like an email address.</strong>")
      })
      
    } else {
      
    output$email_val_warning = renderText({ })
    }
  }
  
  #DISPLAY WARNING IF USER DOESN'T ENTER VALID SURVEYORS NAMES > LENGTH 1
  if(isTruthy(input$surveyors_names2) &&
     sub_reactives$isNextOpen == FALSE &&
     nchar(input$surveyors_names2) > 1) {
    
    output$surveyors_val_warning = renderText({
      HTML("<strong>Important: Please provide full, capitalized names for all surveyors, separated by commas. Type \'NA\' for each anonymous surveyor.</strong>")
    })
  } else {
    
    #IF THE USER HASN'T PUT ANYTHING, RENDER THE WARNING.
    if(input$surveyors_names2 == "") {
      
      output$surveyors_val_warning = renderText({
        HTML("<strong>Important: Please enter the surveyors\' names. This is a required question; the \'Next\' button below this question will remain locked until you have entered an answer of sufficient length.</strong>")
      })
      
    } else {
    
    output$surveyors_val_warning = renderText({ })
    }
  }
  
  #DISPLAY WARNING IF USER DOESN'T ENTER VALID DATE
  if(sub_reactives$dupe_check == FALSE) { #DON'T MAKE THIS CHECK IF WE THINK WE HAVE A DUPE INSTEAD--THAT TAKES PRECIDENCE.
  if(isTruthy(input$survey_date2) &&
     sub_reactives$isNextOpen == FALSE) {
    
    output$date_val_warning = renderText({
      HTML("<strong>Important: Please provide a 10-character date in MM-DD-YYYY format. Also, ensure your date is after 1980 but before today\'s date.</strong>")
    })
  } else {
    output$date_val_warning = renderText({ })
   }
  }
  
  #DISPLAY WARNING IF USER HAS NOT PICKED ANYTHING YET FOR THE DEPTH UNITS
  if(isTruthy(input$depth_units2) &&
     input$depth_units2 == "No selection") {
    
    output$depth_unit_warning = renderText({
      HTML("<strong>Important: Please make a selection. This is a required question; the \'Next\' button below this question will remain locked until you have made a choice.</strong>")
    })
    
  } else {
    output$depth_unit_warning = renderText({})
  }

#DISPLAY WARNING IF USER HAS NOT PICKED ANYTHING YET FOR THE RAKE UNITS
if(isTruthy(input$rake_units2) &&
   input$rake_units2 == "No selection") {
  
  output$rake_unit_warning = renderText({
    HTML("<strong>Important: Please make a selection. This is a required question; the \'Next\' button below this question will remain locked until you have made a choice.</strong>")
  })
  
} else {
  output$rake_unit_warning = renderText({})
}

  #DISPLAY WARNING IF USER DOESN'T SUBMIT VALID SURVEY FILE FORMAT.
  if(isTruthy(input$data_file2) &&
     sub_reactives$isNextOpen == FALSE) {
    output$file_type_warning = renderUI({
      HTML("<strong>Warning: Only .tsv, .csv, .zip (containing a .shp file), and .xls(x) files are supported! Also, don\'t submit Excel files containing multiple sheets; the app only processes the first sheet. This is a required question; the \'Next\' button below this question will not unlock until a valid file has been uploaded.</strong>")
    })
  } else {
    if(!isTruthy(input$data_file2)) {
      
      output$file_type_warning = renderUI({
        HTML("<strong>Important: Please upload a valid survey file. This is a required question; the \'Next\' button below this question will not unlock until a valid file has been uploaded.</strong>")
      })
      
    }
    
    output$file_type_warning = renderUI({ })
  }
  
  
  #DISPLAY WARNING IF USER DOESN'T ENTER STARTING ROW
  if(isTruthy(input$start_row2) &&
     sub_reactives$isNextOpen == FALSE) {
    
    output$row_start_warning = renderText({
      HTML("<strong>Important: Enter a valid starting row value. This is a required question; the \'Next\' button below this question will not unlock until a valid value has been chosen (such values are those between 1 and the number of rows in your survey file).</strong>") 
    })
  } else {
    output$row_start_warning = renderText({ })
  }
  
  #DISPLAY WARNING IF USER DOESN'T ENTER ENDING ROW
  if(isTruthy(input$end_row2) &&
     sub_reactives$isNextOpen == FALSE) {
    
    output$row_end_warning = renderText({
      HTML("<strong>Important: Enter a valid ending row value. This is a required question; the \'Next\' button below this question will not unlock until a valid value has been chosen (such values are those between 1 and the number of rows in your survey file).</strong>") 
    })
  } else {
    output$row_end_warning = renderText({ })
  }
  
  #DISPLAY WARNING IF USER HAS NOT PICKED ANYTHING YET FOR THE FLIP TABLE QUESTION
  if(isTruthy(input$flip_table2) &&
     input$flip_table2 == "No selection") {
    
    output$flip_table_warning = renderText({
      HTML("<strong>Important: Please make a selection. This is a required question; the \'Next\' button below this question will remain locked until you have made a choice.</strong>")
    })
    
  } else {
    output$flip_table_warning = renderText({})
  }
  
  #DISPLAY WARNING IF USER HAS NOT PICKED ANYTHING YET FOR THE COLUMN NAME QUESTION
  if(isTruthy(input$rename_cols2) &&
     input$rename_cols2 == "No selection") {
    
    output$column_name_warning = renderText({
      HTML("<strong>Important: Please make a selection. This is a required question; the \'Next\' button below this question will remain locked until you have made a choice.</strong>")
    })
    
  } else {
    output$column_name_warning = renderText({})
  }
  
  #DISPLAY WARNING IF USER HAS NOT PICKED ANYTHING YET FOR THE COLUMN NAME COMMENTS
  if(input$rename_comms2 == "") {
    
    output$column_val_warning = renderText({
      HTML("<strong>Important: Please enter an answer. This is a required question; the \'Next\' button below this question will remain locked until you have given us some information about the columns that need renaming.</strong>")
    })
    
  } else {
    output$column_val_warning = renderText({})
  }
  
  #DISPLAY WARNING IF USER DOESN'T SUBMIT VALID LOCATION FILE FORMAT.
  if(isTruthy(input$location_file2) &&
     sub_reactives$isNextOpen == FALSE) {
    
    output$loc_file_warning = renderUI({
      tags$span("<strong>Warning: Only .csv, .tsv, .xls(x), and .zip files (containing a .shp file and its companion files) are supported!</strong>")
    })
  } else {
    output$loc_file_warning = renderUI({ })
  }
  
  #DISPLAY WARNING IF USER INDICATES THEY ARE PROVIDING UTM DATA
  if(isTruthy(input$location_data2) &&
     input$location_data2 %in% c("Yes, my raw data file contains UTM data for every point.",
                                "Yes, my raw data file contains both lat-long and UTM data for every point.",
                                "No, my data file doesn't contain location data, but I can upload an additional file containing those location data.")) {
    
    output$utm_warning = renderUI({
     HTML("<strong>Important: You have indicated you might be providing UTM location data, which require knowing the projection used. We\'ll indicate that your data use the NAD83 / UTM Zone 15 North projection (CRS code 26915) unless told otherwise. If that isn\'t right, or if you aren\'t sure, let us know in the comments box on question 19 (this is not relevant if you are providing lat-long data instead or in addition).</strong>")
    })
  } else {
    
    if(isTruthy(input$location_data2) &&
       input$location_data2 == "No selection") {
      
      output$utm_warning = renderUI({
        HTML("<strong>Important: Please make a selection. This is a required question; the \'Next\' button below this question will remain locked until you have made a choice.</strong>")
      })
      
    } else {
    
    output$utm_warning = renderUI({ })
    }
  }
  
})


#A SPECIFIC OBSERVER FOR TRACKING IF A SURVEY MIGHT BE A DUPLICATE AND DISPLAYS A WARNING MESSAGE IN THAT SPECIFIC CASE AND LOCKS THE PROGRESS.
observeEvent(input$survey_date2, priority = -1, {

  #DON'T PROCEED IF THESE DON'T EXIST...PROBABLY UNNECESSARY
  req(input$survey_date2)
  req(input$lake_name2)
  
  #OPEN UP THE DB FILE TO SEE IF WE HAVE ANY RECORDS FROM THIS SAME DOW AND SURVEY_START DATE...
  duplicate_check = db_path %>% 
    open_dataset() %>% 
    select(DOW, SURVEY_START) %>% 
    filter(DOW == input$lake_name2) %>% 
    filter(SURVEY_START == input$survey_date2) %>% 
    collect()
  
  #IF WE DO, WE'LL HAVE NON-0 ROWS AND WE'LL TRIGGER A STOP VIA VALIDATION
  if(nrow(duplicate_check) > 0) {  
    
    output$date_val_warning = renderText({
      HTML("<strong>Warning: Our records indicate we may already have a survey from the lake you selected in Question #5 and the survey start date you selected. Double-check that we don't already have the survey you are trying to submit before proceeding. The \'Next\' button below this question will now remain locked until you back up and change your answers to previous questions.</strong>")
    })
    sub_reactives$isNextOpen = FALSE
    sub_reactives$dupe_check = TRUE
  } else {
    #OTHERWISE, WE CAN GIVE CONTROL BACK TO THE OTHER CHECK VIA TOGGLING DUPE_CHECK. 

    #BY IF GATING HERE, I ONLY WIPE OUT THE EXISTING MESSAGE IF THAT MESSAGE WAS THE ONE ABOUT DUPES.
    if(sub_reactives$dupe_check == TRUE) { output$date_val_warning = renderText({ })
    }
    sub_reactives$dupe_check = FALSE
  }
  
})



# SECTION 3--TAB TRANSITION EVENTS ----------------------------------------


#OBSERVER WATCHING TAB CHANGES, UPDATES INPUTS, RENDERS TABLES, AND UPDATES REACTIVES AS NEEDED.
  observeEvent(input$submission_tabset, {
    
    ##ARRIVING TAB 10
    if(input$submission_tabset == 10) {
      
    sub_reactives$max_start_row = (nrow(sub_reactives$sub_dfV1) - 1) #UPDATE MAX START ROW BASED ON SUBMISSION

    updateNumericInput(session, "start_row2", #UPDATE INPUT FOR THIS TAB
                       max = sub_reactives$max_start_row)
      
      output$sub_tableV2 <- renderDT({ #RENDER SUBMITTED TABLE.
        datatable(sub_reactives$sub_dfV1, 
                  selection = 'none',
                  options = list(scrollX = TRUE, 
                                 searching = FALSE,
                                       fillContainer = T,
                                 language = list(
                                   search = "Search survey records: ",
                                   paginate = list(
                                     "next" = "Next table page",
                                     "previous" = "Previous table page"
                                   ))
                                 ),
                  callback = DT_A11Y_Callback)
      })
    }
    
    ##ARRIVING TAB 11
    if(input$submission_tabset == 11) {

    #DO WE NEED TO CLIP BY START ROW, RE-FASHION COLUMN NAMES?
    if(input$start_row2 == 1) {
      sub_reactives$sub_dfV2 = sub_reactives$sub_dfV1 #NO
      sub_reactives$start_rows_cut_flag = FALSE
    } else {
      
      #YES
      current_df = sub_reactives$sub_dfV1
      current_df = current_df[-(1:(input$start_row2-1)),,drop=F] #CLIP
      
      colnames(current_df) <- as.character(current_df[1,]) #ROW 1 BECOMES COLUMN NAMES
      current_df = current_df[-1,] #ROW 1 DELETE
      current_df = data.frame(current_df, check.names = TRUE) #CORRECT DUPED NAMES
      
      sub_reactives$start_rows_cut_flag = TRUE #FLAG EVENT

      sub_reactives$sub_dfV2 = current_df
    }

    rownames(sub_reactives$sub_dfV2) = NULL #RESET ROW NUMBERING
    
    
    sub_reactives$max_end_row = nrow(sub_reactives$sub_dfV2) #UPDATE MAX END ROW BASED ON SUBMISSION
      
    updateNumericInput(session, "end_row2", #UPDATE INPUT FOR THIS TAB
                       value = nrow(sub_reactives$sub_dfV2),
                       max = nrow(sub_reactives$sub_dfV2))
    
    #FIND LAST PAGE OF TABLE TO SHOW. 

    last_page_start <- ((nrow(sub_reactives$sub_dfV2) - 1) %/%
                          rows_per_page) * rows_per_page #ROW LAST PAGE BEGINS ON. %/% RETURNS ONLY THE INTEGER OF DIVISION, OPPOSITE OF %%. PARENTHESES HERE IMPORTANT!

    output$sub_tableV3 <- renderDT({
      datatable(sub_reactives$sub_dfV2, #RENDER START-CLIPPED TABLE.
                selection = 'none',
                options = list(scrollX = TRUE, 
                               searching = FALSE,
                                     fillContainer = T,
                                     pageLength = rows_per_page, #OVERRIDE ANY USER CHANGES TO PAGE LENGTH
                                     displayStart = last_page_start,
                               language = list(
                                 search = "Search survey records: ",
                                 paginate = list(
                                   "next" = "Next table page",
                                   "previous" = "Previous table page"
                                 ))),
                callback = DT_A11Y_Callback) #SET STARTING VIEW TO LAST PAGE
      })
      
    }
    
    
    ##ARRIVING TAB 12
    if(input$submission_tabset == 12) {
      
      #IF CUTTING ROWS HERE, FLAG
      if(input$end_row2 < nrow(sub_reactives$sub_dfV2)) {
        sub_reactives$end_rows_cut_flag = TRUE
      } else {
        sub_reactives$end_rows_cut_flag = FALSE
      }
      
      sub_reactives$sub_dfV3 = sub_reactives$sub_dfV2[1:input$end_row2, ] #CLIP BY ENDING ROW
      
      output$sub_tableV4 <- renderDT({
        datatable(sub_reactives$sub_dfV3, #RENDER END-CLIPPED TABLE.
                  selection = 'none',
                  options = list(scrollX = TRUE, 
                                 searching = FALSE,
                                       fillContainer = T,
                                 language = list(
                                   search = "Search survey records: ",
                                   paginate = list(
                                     "next" = "Next table page",
                                     "previous" = "Previous table page"
                                   ))),
                  callback = DT_A11Y_Callback)
      })
       
    }
    
    
    ##ARRIVING TAB 13 (OR 15 HAVING SKIPPED OVER THE NON-THROWS QUESTIONS, NEED TO MAKE DFV4)
    if(input$submission_tabset == 13 || 
       (input$submission_tabset == 15 &&
        input$acknowledge_data2 == "Yes, I understand; my current submission will not include any non-throws.")) {
      
      #DO WE NEED TO TRANSPOSE TABLE?
      if(input$flip_table2 == "Yes, flip my table!") {
        #YES
        current_df = sub_reactives$sub_dfV3 
        temp.row1 = colnames(current_df) #GRAB OLD COLUMN NAMES
        
        current_df[] = lapply(current_df, as.character) #CONVERT ALL COLS TO CHARACTER TYPE (SO INTRODUCING CHARACTERS INTO DATE COLS DOESN'T CAUSE ERRORS)
        
        temp_df1 = rbind(temp.row1, current_df) #MAKE ROW 1 OLD COLUMN NAMES
        
        temp_df2 = smart_transpose(temp_df1) #TRANSPOSE TABLE
        
        colnames(temp_df2) <- temp_df2[1,] #TAKE NEW ROW 1, MAKE COLUMN NAMES.
        
        temp_df2 = temp_df2[-1,] #DELETE ROW 1
        
      } else if(input$flip_table2 == "No, don't flip my table!") {
        temp_df2 <- sub_reactives$sub_dfV3 #NO
      }
      
      sub_reactives$sub_dfV4 = temp_df2
      
      output$sub_tableV5 <- renderDT({
        datatable(sub_reactives$sub_dfV4, #RENDER FLIPPED TABLE
                  selection = 'none',
                  options = list(scrollX = TRUE, 
                                 searching = FALSE,
                                       fillContainer = T,
                                 language = list(
                                   search = "Search survey records: ",
                                   paginate = list(
                                     "next" = "Next table page",
                                     "previous" = "Previous table page"
                                   ))),
                  callback = DT_A11Y_Callback)
      })
      
      updateSelectInput(session, "non_throws2", #UPDATE INPUT FOR THIS TAB BASED ON NEW COL NAMES
                        choices = c("No selection", "No non-throws", 
                                    colnames(sub_reactives$sub_dfV4)))
      
    }
    
    
    ##ARRIVING TAB 14
    if(input$submission_tabset == 14) {
      
      #DOES THE NON-THROWS VAL QUESTION NEED POPULATING?
      if(input$non_throws2 == "No non-throws") {
      shinyWidgets::updatePickerInput(session, "non_throws_val2", #NO
                        choices = "Proceed to the next question!", 
                        selected = "Proceed to the next question!") 
      } else {

        #YES
        #EXTRACT ALL UNIQUE VALUES FROM NON-THROWS COL (CAUTION: FIDDLY)
        vals1 <- dplyr::tibble(sub_reactives$sub_dfV4[input$non_throws2]) %>%
          dplyr::pull() %>% 
          as.character() %>% 
          unique()
        
        #CONVERT ANY NA VALS TO "NA" AND "BLANKS" BECAUSE THIS WORKS BETTER AND WHAT PEOPLE EXPECT. 
        if(isTruthy(is.na(vals1))) {
          those = which(is.na(vals1)) #CODE QUIRK--NAS MUST BE REMOVED IN FIDDLY WAY. 
          vals1 = vals1[-c(those)]
          vals1 = c("Blanks", "NA", vals1)
        }
        
        names(vals1) <- vals1 
        
      updatePickerInput(session, "non_throws_val2", #UPDATE INPUT FOR THIS TAB BASED ON AVAILABLE VALUES
                        choices = vals1,
                        selected = vals1[1], #MAKE IT THE FIRST OPTION SO SOMETHING VALID IS SELECTED.
                        label = "Now, tell us what VALUE(S) in the column you chose indicate(s) that a row represents a \'non-throw.\' We'll throw out every row that contains the value(s) selected here. This is a required question; the \'Next\' button below this question will not unlock until at least one valid selection is made.")
      }
     
      output$sub_tableV6 <- renderDT({
        datatable(sub_reactives$sub_dfV4, #RENDER FLIPPED TABLE
                  selection = 'none',
                  options = list(scrollX = TRUE, 
                                 searching = FALSE,
                                       fillContainer = T,
                                 language = list(
                                   search = "Search survey records: ",
                                   paginate = list(
                                     "next" = "Next table page",
                                     "previous" = "Previous table page"
                                   ))),
                  callback = DT_A11Y_Callback)
      })
      
    }
    
    ##ARRIVING TAB 15
    if(input$submission_tabset == 15) {
      
      #DO WE NEED TO REMOVE NON-THROWS?
      if(all(isTruthy(input$non_throws_val2)) &&
         !"Proceed to the next question!" %in% input$non_throws_val2) {
        #YES
      sub_reactives$sub_dfV5 = find.remove.nonthrows(sub_reactives$sub_dfV4, input$non_throws2, input$non_throws_val2)
      } else {
        sub_reactives$sub_dfV5 = sub_reactives$sub_dfV4 #NO
      }
      
      temp_df2 = sub_reactives$sub_dfV5
      
      #NAME REPLACEMENT
      oldcolnames = names(temp_df2) #GRAB OLD COLUMN NAMES
      oldcolnames = tidyName(oldcolnames) #TIDY THEM 
      
      newcolnames = unname(sapply(oldcolnames, colname.replacement)) #LOOKUP REPLACEMENTS
      
      temp_df2[] = lapply(temp_df2, as.character) #CONVERT ALL COLS TO CHARACTERS (TO PREVENT ERRORS W/ DATE COLUMNS)
      
      temp_df3 <- rbind(newcolnames, temp_df2) #INSERT NEW COL NAMES AS ROW 1
      
      #FIND ALL COLUMNS THAT ARE EMPTY 
      not.these = colnames(temp_df2)[ 
        apply(temp_df2, 2, function(x) { 
          all(is.na(x))  |
            all(x == "") | 
            all(is.null(x)) | 
            all(x %in% 0) } )
      ]
      
      #FIND ALL COLS MARKED FOR DELETION
      maybe.delete.these = colnames(temp_df3)[grepl("delete", temp_df3[1,])]
      
      #EXCLUDES EMPTIES FROM DELETED LIST
      delete.these = maybe.delete.these[which(!maybe.delete.these %in% not.these)]
      
      sub_reactives$COLUMNS_DELETE <- delete.these #UPDATE INDICATORS WITH ONLY CONSEQUENTIAL DELETIONS

      if(!isTruthy(sub_reactives$COLUMNS_DELETE)) { sub_reactives$COLUMNS_DELETE <- "None" } #ENSURE INDICATOR IS NEVER EMPTY

      sub_reactives$sub_dfV5 <- temp_df3
      
      #FIND ALL COLS MARKED AS AMBIGUOUS
      ambiguous_cols = which(grepl("ambiguous", 
                                               sub_reactives$sub_dfV5[1,], 
                                               ignore.case = TRUE)
      )
      
      #IF DELETED/AMBIGUOUS COLUMNS, ASSEMBLE TEXT WARNING TO USERS. 
      if(!"None" %in% sub_reactives$COLUMNS_DELETE |
         length(ambiguous_cols) > 0) {
        
        ambigs = paste0("<span class = 'ambig_text'>",
                        colnames(sub_reactives$sub_dfV5)[ambiguous_cols],
                        "</span>", 
                        collapse = ",<br>")
        
        deleters = paste0("<span class = 'delete_text'>",
                          sub_reactives$COLUMNS_DELETE, 
                          "</span>", 
                          collapse = ",<br>")
        
        ambigs = gsub("None", "", ambigs) #SO SPOTS ALWAYS HAVE SOME CONTENT
        deleters = gsub("None", "", deleters)
        
        text2say = paste0(
          deleters,
          ",<br>",
          ambigs
        )
        
        text2say = gsub("None,<br>", "", text2say) #PREVENT STRAY "NONES"
        text2say = gsub(",<br>,<br>", ",<br>", text2say) #ELIMINATE ANY STRAY LINE BREAKS
      } else {
        text2say = "None"
      }
      
      #FIGURE OUT WHICH COLUMNS TO MAKE RED
      delete_cols = which(colnames(sub_reactives$sub_dfV5) %in% 
                                  sub_reactives$COLUMNS_DELETE)
      
      #UPDATE REACTIVE TRACKERS--STORES THIS CONTENT FOR USE ON NEXT TAB FOR "FREE."
      sub_reactives$delete_message = text2say
      sub_reactives$delete_cols = delete_cols
      sub_reactives$ambiguous_cols = ambiguous_cols
      
      #DISPLAY WARNING ABOUT AMBIGUOUS/DELETE COLUMNS
      if(sub_reactives$delete_message != "None") {
        output$columns_delete2a <- shiny::renderText({
          shiny::HTML("<span class = 'delete_ambig_header'><strong>Note: For your reference, all columns marked for deletion are now shown in red. Any columns marked as ambiguous are now shown in blue. If any of these column contain usable data, please indicate that there are some issues with our new names.</strong><br><br></span>", sub_reactives$delete_message)
        })
      } else {
        shinyjs::hide("columns_delete2a")
      }
      
      output$sub_tableV7 <- DT::renderDT({
        DT::datatable(sub_reactives$sub_dfV5, #RENDER NON-THROWS-CLIPPED TABLE, WITH HIGHLIGHTS FOR DELETE/AMBIG COLS.
                      options = list(scrollX = TRUE, 
                                     searching = FALSE,
                                           fillContainer = T,
                                     language = list(
                                       search = "Search survey records: ",
                                       paginate = list(
                                         "next" = "Next table page",
                                         "previous" = "Previous table page"
                                       ))),
                      callback = DT_A11Y_Callback) %>% 
          DT::formatStyle(sub_reactives$delete_cols, color = "white", backgroundColor = "darkred") %>%  
          DT::formatStyle(sub_reactives$ambiguous_cols, color = "white", backgroundColor = "blue")
      })
      
    }
    
    ##ARRIVING TAB 16
    if(input$submission_tabset == 16) {
      
      #RE-RENDER WARNING HERE IF NECCESARY
      if(sub_reactives$delete_message != "None") {
        output$columns_delete2b <- shiny::renderText({
          shiny::HTML("<span class = 'delete_ambig_header'> For your reference, all columns marked for deletion are now shown in red. Any columns marked as ambiguous are now shown in blue. If any of these column contain usable data, please clarify what data these columns contain! <br><br></span>", sub_reactives$delete_message)
        })
      } else {
        shinyjs::hide("columns_delete2b")
      }

      #RE-RENDER HIGHLIGHTED TABLE HERE, IF NECESSARY 
      output$sub_tableV8 <- DT::renderDT({
        DT::datatable(sub_reactives$sub_dfV5,
                      options = list(scrollX = TRUE, 
                                     searching = FALSE,
                                           fillContainer = T,
                                     language = list(
                                       search = "Search survey records: ",
                                       paginate = list(
                                         "next" = "Next table page",
                                         "previous" = "Previous table page"
                                       ))),
                      callback = DT_A11Y_Callback) %>% 
          DT::formatStyle(sub_reactives$delete_cols, color = "white", backgroundColor = "darkred") %>%  
          DT::formatStyle(sub_reactives$ambiguous_cols, color = "white", backgroundColor = "blue")
      })
      
    }
    
    ##ARRIVING TAB 17
    if(input$submission_tabset == 17) {
     
      current_dftobe <- sub_reactives$sub_dfV5[-1,] #SET ASIDE ALL BUT ROW 1.
      names_obj = unname(unlist(sub_reactives$sub_dfV5[1,, drop = T])) #GET ROW 1 AS VECTOR
      
      #FIND AND REMOVE COMPLETELY EMPTY COLUMNS
      current_dftobe2 = current_dftobe %>%
        dplyr::filter(!apply(., 1, function(x) all(is.na(x)))) %>%
        dplyr::filter(!apply(., 1, function(x) all(x == ""))) %>%
        dplyr::filter(!apply(., 1, function(x) all(is.null(x)))) %>%
        dplyr::filter(!apply(., 1, function(x) all(x %in% 0)))
      
      #UPDATE TRACKERS WITH THESE
      sub_reactives$COLUMNS_EMPTY <- names(current_dftobe)[ #NAMES IN PRECUT DF NOT IN POST-CUT DF.
        !names(current_dftobe) %in% names(current_dftobe2)
      ] 
      
      if(!isTruthy(sub_reactives$COLUMNS_EMPTY)) { sub_reactives$COLUMNS_EMPTY <- "None" } #ENSURE NOT EMPTY
      
      #ELIMINATE NAMES OF EMPTY COLUMNS FROM REPLACEMENT PROCESS
      names2delete = which(!names(current_dftobe) %in% names(current_dftobe2))
      
      current_dftobe3 = current_dftobe2
      
      #OVERRIDE OLD COLUMN NAMES WITH OUR NEW TIDIED ONES FROM OLD ROW 1. WARNING: FIDDLY. IF NO DELETIONS, CAN CAUSE BIG PROBLEMS.
      if(isTruthy(names2delete)) {
        names(current_dftobe3) = tidyName(
          names_obj[-names2delete]
        ) 
      } else {
        names(current_dftobe3) = tidyName(names_obj)  
      }
      
      current_dftobe3 <- data.frame(current_dftobe3, check.names = TRUE) #FIX ANY DUPED NAMES
      
      current_dftobe3 = current_dftobe3[, !grepl("delete", colnames(current_dftobe3))] #DELETE ALL 'DELETE' COLUMNS

      ###CHECKS RELATED TO DEPTH DATA--WE NOW ULTIMATELY WANT ALL DATA PASSED THRU HERE TO BE IN FEET ONLY.
      if(length(which(grepl("depth", names(current_dftobe3), ignore.case = T))) != 0) { #ARE THERE DEPTH DATA AT ALL?
        
        if(length(which(grepl("depth", names(current_dftobe3), ignore.case = T))) == 1) { #IF THERE IS ONLY ONE COLUMN OF DEPTH DATA...
          if(input$depth_units2 == "Meters") { #AND IT'S IN METERS...
            
            #MULTIPLY TO CONVERT THESE TO FEET AND OVERRIDE, BUT DON'T DO SO FOR ANY TEXT-BASED ENTRIES. THOSE, WE'LL LEAVE ALONE AND PLAN TO CATCH IN QA/QC.
            current_dftobe3$depth_ft[!is.na(as.numeric(current_dftobe3$depth_ft))] = 
              round(as.numeric(current_dftobe3$depth_ft)[!is.na(as.numeric(current_dftobe3$depth_ft))] * 3.28084, 2)
            
          }
        } else {
          
          #OTHERWISE, IF THERE'S EXACTLY TWO COLUMNS OF DEPTH DATA...
          if(length(which(grepl("depth", names(current_dftobe3), ignore.case = T))) == 2) {
            #FIGURE OUT IF, GENERALLY, THE FIRST COLUMN IS HIGHER THAN THE SECOND 
            leftHigher = length(which(as.numeric(current_dftobe3$depth_ft) > 
                                         as.numeric(current_dftobe3$depth_ft.1)))
            rightHigher = length(which(as.numeric(current_dftobe3$depth_ft) < 
                                         as.numeric(current_dftobe3$depth_ft.1)))
            
            if(leftHigher > rightHigher) { current_dftobe3$depth_ft.1 = NULL 
            }  else {#NIX THE RIGHT COLUMN AS PUTATIVELY METERS. YES, IF USERS WERE BOTCHING THINGS AND HAD MIXED COLUMNS, THIS WOULD HAPHAZARDLY HIDE THAT FACT, BUT (SHRUGS)

              current_dftobe3$depth_ft = current_dftobe3$depth_ft.1 #MOVE THE DATA OTHERWISE
              current_dftobe3$depth_ft.1 = NULL
              }
          }
        }
      }
      
      sub_reactives$sub_dfV6 <- current_dftobe3
      
      output$sub_tableV9 <- DT::renderDT({ #RENDER POST-RENAMING TABLE
        DT::datatable(sub_reactives$sub_dfV6,
                      options = list(scrollX = TRUE, 
                                     searching = FALSE,
                                           fillContainer = T,
                                     language = list(
                                       search = "Search survey records: ",
                                       paginate = list(
                                         "next" = "Next table page",
                                         "previous" = "Previous table page"
                                       ))),
                      callback = DT_A11Y_Callback)
      })
      
    }
    
    ##ARRIVING TAB 18
    if(input$submission_tabset == 18) {
    output$sub_tableV10 <- DT::renderDT({ #RENDER POST-RENAMING TABLE
      DT::datatable(sub_reactives$sub_dfV6,
                    options = list(scrollX = TRUE, 
                                   searching = FALSE,
                                         fillContainer = T,
                                   language = list(
                                     search = "Search survey records: ",
                                     paginate = list(
                                       "next" = "Next table page",
                                       "previous" = "Previous table page"
                                     ))),
                    callback = DT_A11Y_Callback)
    })
    
    }
    
    ##ARRIVING TAB 19
    if(input$submission_tabset == 19) {

      #FIND TAXONOMIC OPTIONS
      taxonomic_names = tidyName(
        unique(newfieldnames$newfieldname[newfieldnames$taxonomic == "Y"])
      )
      
      which_taxonomic = which(colnames(sub_reactives$sub_dfV6) %in% taxonomic_names) #DETERMINE WHICH COLS ARE TAXONOMIC
      
      df2check = data.frame(sub_reactives$sub_dfV6[,which_taxonomic]) #CUT TO JUST TAX COLS
      df2check = data.frame(sapply(df2check, function(x) { gsub("\xc2\xa0", "", x, fixed=T) } )) #GET RID OF ALL NON-BREAKING SPACES
      df2check[is.na(df2check)] = 0 #REPLACE ALL NATURAL NAS W/ 0S
      
      text_check = data.frame(convert_column_types(df2check)) #RECLASS ALL COLUMNS TO NUMERIC IF POSSIBLE
      
        #CREATE TEXT WARNING TO USERS BY FINDING ANY COLS THAT CAN'T CONVERT
        anytextcols = colnames(
          text_check[, sapply(text_check, class) == "character", 
                                 drop = F])
        
        #ID COLS TO MARK RED AS CONTAINING TEXT
        cols2red = which(colnames(sub_reactives$sub_dfV6) %in% anytextcols)
        
        if(isTruthy(sub_reactives$current_path)) { #SHOW MESSAGE BUT NOT ON RESET
          shinyjs::show("sub_tableV11")
        }
        
        #SHOW TABLE
        output$sub_tableV11 <- DT::renderDT({
          DT::datatable(sub_reactives$sub_dfV6, 
                        selection = 'none',
                        options = list(scrollX = TRUE, 
                                       searching = FALSE,
                                       fillContainer = T,
                                       language = list(
                                         search = "Search survey records: ",
                                         paginate = list(
                                           "next" = "Next table page",
                                           "previous" = "Previous table page"
                                         ))),
                        callback = DT_A11Y_Callback) %>% 
            DT::formatStyle(cols2red,
                            color = "white", 
                            backgroundColor = "purple")
        })
        
        #RENDER MESSAGE TO DISPLAY TO USER
        if(isTruthy(anytextcols)) {
          shinyjs::show("texty_columns2")
          
          cols2report = paste0(anytextcols, collapse = ",<br>")
          
          output$texty_columns2 <- shiny::renderText({
            shiny::HTML("<strong>Note: The app has detected at least one text-based (non-numeric) entry in the following taxonomic columns, also marked in purple in the table to the right. Text values may be appropriate in several circumstances (e.g., marking taxa as having been visually identified but not captured on the rake), but we still need to know what these text values mean, so please tell us above in the comments box! Note that text values should NOT be used to represent missing data or absences. In those instances, it's best to use NA. If you are unsure if your text-based data are desirable or not, write to Alex Bajcz (bajcz003@umn.edu) at MAISRC to ask before submitting.</strong><br><br>", cols2report)  
          })
        } else {
          shinyjs::hide("texty_columns2"); output$texty_columns2 <- shiny::renderText({ "" })
        }

      updateActionButton(session, "new_sub_next", "Submit!") #CONVERT NEXT TO SUBMIT
      
    } else { updateActionButton(session, "new_sub_next", "Next") } #UNDO CONVERSION
    
  })
  

# SECTION 4--NEXT BUTTON OBSERVER + SUBMISSION ----------------------------


  ##OBSERVER FOR NEXT BUTTON PRESSES, HANDLES ADVANCEMENT AND SUBMISSION
  observeEvent(next_button_debounce(), 
               priority = 2, {
    
    currTab = as.numeric(input$submission_tabset)
    disable("new_sub_next") #WE WANT TO DISABLE THE BUTTON AS SOON AS THE DEBOUNCE REGISTERS SO THAT USERS CANNOT HIT THE NEXT BUTTON MULTIPLE TIMES TO SKIP QUESTIONS.

    #IN ONE INSTANCE, PENDING CERTAIN RESPONSES, MAY SKIP 3 TABS AHEAD
    if(currTab == 12 &
       input$acknowledge_data2 == "Yes, I understand; my current submission will not include any non-throws.") {
      nextTab = min(currTab+3, 19)
      
      slideIn(currTab, nextTab, numQuestions)
      
      updateTabsetPanel(session, "submission_tabset", 
                        selected = as.character(nextTab))

    } else {
    
    #SOMETIMES, PENDING VALIDATION, MAY SKIP 2 TABS AHEAD
      if((currTab == 13 & 
         input$non_throws2 == "No non-throws") |
         (currTab == 15 &
         input$rename_cols2 == "Yes, looks good!") |
         (currTab == 17 &
         input$location_data2 != "No, my data file doesn't contain location data, but I can upload an additional file containing those location data.")) {

        nextTab = min(currTab+2, 19)
        
        slideIn(currTab, nextTab, numQuestions)
        
        updateTabsetPanel(session, "submission_tabset", 
                          selected = as.character(nextTab))
      } else {
      
      if(currTab != 19) {
        
      nextTab = min(currTab+1, 19)
      
      slideIn(currTab, nextTab, numQuestions)
        
      updateTabsetPanel(session, "submission_tabset", 
                        selected = as.character(nextTab))
      
      } else {

##SUBMISSION--ONLY CONSEQUENCE OF HITTING NEXT/SUBMIT UPON TAB  --------

        
        disable("new_sub_next") #DISABLE THE BUTTON RIGHT AWAY.
        
        shiny::showNotification("We're working on submitting your files and data--please wait!", #NOTIFY USER SUBMISSION HAS BEGUN
                                type = "message",
                                id = "submitting_note2",
                                duration = Inf)
        
        session$sendCustomMessage("a11yLiveMessage", 
                                  "We are submitting your files now. Please wait.") #THIS WILL BE READ ALOUD BY SCREEN READERS AND WILL OTHERWISE NOT BE VISIBLE.
        
        sub_reactives$sub_complete = TRUE #UPDATE TRACKER TO CONTROL NEXT/BACK STATUS

        #CHECK FOR 2 DIGIT YEAR, CONVERT TO 4 DIGIT.
        if(stringr::str_starts(input$survey_date2, "00")) {
          date2use = stringr::str_replace(input$survey_date2, "00", "20")
        } else {
          date2use = input$survey_date2
        }

        #CHECK FOR 7 DIGIT DOW, APPEND LEADING 0
        current_lake <- input$lake_name2
        current_lake2 <- current_lake
        if(nchar(current_lake) == 7) { current_lake2 <- paste0("0", current_lake) } 
        
        #BEGIN COMPILING METADATA FOR SUBMISSION
        metadata_df = dplyr::tibble(
          submitter_name = input$submitter_name2,
          submitter_email = input$submitter_email2,
          surveyors = input$surveyors_names2,
          DOW = current_lake2,
          survey_dateStart = date2use,
          start_row = input$start_row2,
          end_row = input$end_row2,
          start_col = "", #OLD COLS. KEEP SPOTS IN METADATA FILE OCCUPIED.
          end_col = "",
          flip_table = input$flip_table2,
          rename_cols = input$rename_cols2,
          rename_comms = input$rename_comms2,
          depth_units = input$depth_units2, #KEPT NOW ONLY FOR ASSISTING W/ QA/QC
          rake_units = input$rake_units2,
          non_throws = input$non_throws2,
          non_throws_val = toString(input$non_throws_val2, sep=", "), 
          location_data = input$location_data2,
          gen_comms = input$gen_comms2,
          submit_date = as.character(.POSIXct(Sys.time(), tz = "America/Chicago"))) 
        
        #BEGIN UPLOAD PROCESS FOR (UP TO) 3 FILES. RAW SUBMITTED FILE 1ST
        rawfile4upload = input$data_file2
        
        prepared1 = prepare_GDrive_upload(rawfile4upload$datapath) #PREPARE

        metadata_df$raw_file = prepared1$name.me #RECORD UNIQUE FILE NAME FOR METADATA
        
        #BEGIN UPLOAD LOCATION FILE (IF APPLICABLE).
        if(isTruthy(input$location_file2) &&
           input$location_data2 == "No, my data file doesn't contain location data, but I can upload an additional file containing those location data.") {
          
          locfile4upload = input$location_file2 #POINTER
          
          prepared2 <<- prepare_GDrive_upload(locfile4upload$datapath) #PREPARE
          
          metadata_df$location_file = prepared2$name.me #RECORD UNIQUE FILE NAME FOR METADATA
          
        } else {
          metadata_df$location_file = NA #RECORD NA OTHERWISE
          prepared2 <<- "No location file"
        }

        #BEGIN UPLOAD OF CLEANED FILE 
        cleanfile4upload = sub_reactives$sub_dfV6
        
        #APPEND NEW METADATA TO CLEANED FILE
        cleanfile4upload$SUBMITTER_NAME = input$submitter_name2
        cleanfile4upload$SUBMITTER_EMAIL = input$submitter_email2
        cleanfile4upload$DOW = input$lake_name2
        cleanfile4upload$SURVEY_START = date2use
        cleanfile4upload$RAKE_MAX = input$rake_units2
        cleanfile4upload$SUBMIT_TIME = as.character(.POSIXct(Sys.time(), tz = "America/Chicago")) 
        cleanfile4upload$SURVEYORS = input$surveyors_names2

        #DEPTH CHECKS. IF DEPTH COLUMN (IF ANY) "EMPTY", SILENTLY DELETE, OVERRIDE METADATA OF DEPTH UNITS TO NONE. ALSO, FLIP NEGATIVES TO POSITIVE. 
        if("depth_ft" %in% colnames(cleanfile4upload)) {
          
          if(all( #CHECK EMPTY -- NO ISTRUTHY CUZ CONSISTENCY IN EMPTINESS IS KEY.
            cleanfile4upload$depth_ft %in% 0 |
            is.null(cleanfile4upload$depth_ft) |
            is.na(cleanfile4upload$depth_ft) |
            cleanfile4upload$depth_ft %in% "")) {
            
            #OVERRIDE METADATA WHEN YES
            metadata_df$depth_units = "None"
            
            #DELETE COL, UPDATE TRACKERS
            cleanfile4upload$depth_ft = NULL
            sub_reactives$COLUMNS_EMPTY <- c("depth", sub_reactives$COLUMNS_EMPTY)
            
          } else { #CHECK FOR NEGATIVE VALUES
            if(any(as.numeric(cleanfile4upload$depth_ft) < 0, na.rm=T)) { 
              
              #FLIP POSITIVE IF YES
              cleanfile4upload$depth_ft[as.numeric(cleanfile4upload$depth_ft) < 0, na.rm = T] = 
                abs(as.numeric(cleanfile4upload$depth_ft)[as.numeric(cleanfile4upload$depth_ft) < 0, na.rm = T] )
            }
          } 
        } else {
          #CHECK IF NO DEPTH COL, OVERRIDE METADATA ON YES
          metadata_df$depth_units = "None"
        }
        
        #GET TEMP PATH AND FILE NAME
        tmp.path3 = file.path(paste0(tempdir(), "\\")) 
        name.me3 = paste0(paste0(sample(c(LETTERS, 0:9), 20), collapse=""), ".", "csv", sep = "")
        
        #IF THE USER PROVIDED UTM DATA, WE ARE NOW GOING TO TRANSITION THOSE TO LAT/LONG DATA FOR EASE OF STORAGE AND USE. 
        if(all(c("x", "y") %in% names(cleanfile4upload)) &
           any(!c("latitude", "longitude") %in% names(cleanfile4upload))) { #IF X AND Y DATA WERE PROVIDED AND NOT A COMPLETE SET OF LAT/LONG DATA WAS...
          
          utm_sf = st_as_sf(cleanfile4upload, 
                            coords = c("x", "y"),
                            crs = 26915) #MAKE THE SF OBJECT. IF THIS IS THE WRONG CRS, WE'LL HAVE TO CORRECT THAT IN QA/QC I GUESS.
          
          utm_sf = st_transform(utm_sf, crs=4236) #TURN IT INTO LAT/LONG CRS
          
          latLongCoords = st_coordinates(utm_sf) #PULL THE LAT/LONG COLS OUT.
          
          #PREALLOCATE THESE COLUMNS, BUT ONLY IF THEY ARE NEEDED.
          if(!"latitude" %in% names(utm_sf)) { utm_sf$latitude = NA }
          if(!"longitude" %in% names(utm_sf)) { utm_sf$longitude = NA }
          
          #PLACE DATA WHERE THEY OUGHT TO GO.
          utm_sf$latitude = latLongCoords[, "Y"] 
          utm_sf$longitude = latLongCoords[, "X"]

          cleanfile4upload = st_drop_geometry(utm_sf) #ELIMINATE GEOMETRY
          cleanfile4upload$x = NULL #ELIMINATE THESE COLUMNS.
          cleanfile4upload$y = NULL
          
        }
        
        #WRITE CLEAN FILE TO CSV
        utils::write.csv(cleanfile4upload, 
                         file = paste0(tmp.path3, name.me3), row.names = F)
        
        #GENERATE METADATA QA/QC FLAG IN RE: IF STATION NUMBERS ARE NON-CONSECUTIVE.
        if(isTruthy(
          grepl(pattern = "sta_nbr", 
                      names(cleanfile4upload), 
                      ignore.case = T))) {
          
          #CHECK IF ALL NUMBERS ARE CONSECUTIVE & START AT 1 OR 0.  
          if(all(diff(sort(as.numeric(cleanfile4upload$sta_nbr))) == 1) &
             any(c(0,1) %in% cleanfile4upload$sta_nbr)) { 
            consec_rows = TRUE
          } else {
            consec_rows = FALSE
          }
        } else {
          consec_rows = "No station numbers" #PREVENT EMPTY FLAG.
        }
        
        metadata_df$clean_file = name.me3

        #CROSS REF COLUMNS_DELETE AND COLUMNS_EMPTY, MOVE DUPES TO LATTER ONLY. 
        if(isTruthy(sub_reactives$COLUMNS_DELETE %in% sub_reactives$COLUMNS_EMPTY)) {
          
          sub_reactives$COLUMNS_DELETE = 
            sub_reactives$COLUMNS_DELETE[-(sub_reactives$COLUMNS_DELETE %in% 
                                             sub_reactives$COLUMNS_EMPTY)]
        }
        
        if(!isTruthy(sub_reactives$COLUMNS_DELETE)) { sub_reactives$COLUMNS_DELETE <- "None" } #PREVENT EMPTY FLAG.
        
        #RE-FORMAT COLUMNS_DELETE TO BE QA/QC FLAG. 
        metadata_df$COLUMNS_DELETED <- paste0(
          paste0(c(
            paste0("Deleted columns: ", paste0(sub_reactives$COLUMNS_DELETE, collapse=", ")),
            paste0("Empty columns: ", paste0(sub_reactives$COLUMNS_EMPTY, collapse = ", "))), 
            collapse = "; ")
        )
        
        #CREATE QA/QC FLAG FOR COLS CONTAINING TEXT...
        current_df = sub_reactives$sub_dfV6
        
        current_df = utils::type.convert(current_df, as.is = T) #COERCE ALL COLS TO NUMERIC FIRST...DEETS --> https://statisticsglobe.com/change-classes-data-frame-columns-automatically-r
        anytextcols = colnames(current_df[, sapply(current_df, class) == "character", drop = F]) #MAKE VEC OF STILL-CHARACTER COLS. 
        metadata_df$COLUMNS_WTEXT <- paste0(anytextcols, collapse=", ") #COLLAPSE STRING
        
        #APPEND AS FLAG.
        metadata_df$CONSEC_SITES = consec_rows

        #DO CHECK TO SEE IF TOO LOW A RAKE SCORE VAL WAS PROVIDED.
        if(isTruthy(as.numeric(input$rake_units2))) { #IS THE INPUT CHOICE NUMERIC
          
          taxonomic_names = tidyName(
            unique(newfieldnames$newfieldname[newfieldnames$taxonomic == "Y"])
            ) #FIND TAXONOMIC OPTIONS
          
          which_taxonomic = which(colnames(cleanfile4upload) %in% taxonomic_names) #DETERMINE WHICH COLS ARE TAXONOMIC
          
          only_taxa_cols = cleanfile4upload[, c(which_taxonomic)] #CUT TO ONLY TAXONOMIC COLS
          
          only_taxa_vec = unique(suppressWarnings(as.numeric(unlist(only_taxa_cols)))) #CONVERT TO NUMERIC VECTOR, ONLY UNIQUE VALS
          
          nas2remove = which(is.na(only_taxa_vec)) #REMOVE NAS IF APPLICABLE
          if(isTruthy(nas2remove)) {
          only_taxa_vec = only_taxa_vec[-nas2remove]
          }
          
          if(any(only_taxa_vec > as.numeric(input$rake_units2))) {
            metadata_df$rake_unit_flag = TRUE #IF VAL OBSERVED THAT IS HIGHER THAN MAX INDICATED, FLAG TRUE 
          } else {
            metadata_df$rake_unit_flag = FALSE #ELSE FALSE
          } 
 
        } else {
          metadata_df$rake_unit_flag = FALSE #ELSE FALSE
        }
        
        #FLAG TRACKING WHETHER ROWS WERE CUT
        if(sub_reactives$start_rows_cut_flag == TRUE) {
          if(sub_reactives$end_rows_cut_flag == TRUE) {
            metadata_df$rows_cut_flag = "BOTH"
          } else {
            metadata_df$rows_cut_flag = "START"
          }
        } else {
          if(sub_reactives$end_rows_cut_flag == TRUE) {
            metadata_df$rows_cut_flag = "END"
          } else {
            metadata_df$rows_cut_flag = "NONE"
          }
        }

        #PREPARE CLEAN FILE FOR USER DOWNLOAD.
        cleanfile4upload = cleanfile4upload %>% 
          dplyr::relocate(SUBMITTER_NAME:SURVEYORS) #RELOCATE METADATA COLS TO FAR LEFT.
        
        #POP UP INDICATING SUCCESS
        shiny::showNotification("Your data and files have been submitted! Thanks! Hit the button on screen to reset the form to submit more data or you can close this page. You can also download your cleaned data file using the download button on screen.",
                                type = "message",
                                id = "submitted_note2",
                                closeButton = TRUE,
                                duration = 30)
        
        session$sendCustomMessage("a11yLiveMessage", 
                                  "Your submission was successful. Thanks! Hit the reset button now on screen to submit again. There is also a download button on screen now if you would like to download your cleaned data file.") #FOR SCREEN READERS.
        
        shiny::removeNotification("submitting_note2")
        
        #SHOW DOWNLOAD BUTTON
        shinyjs::show("download_now2")
        
        #ADD DOWNLOAD HANDLER FOR ABOVE
        output$download_now2 <- shiny:::downloadHandler("cleaned.csv", function(file) { 
          utils::write.csv(cleanfile4upload, file, row.names = F)
        })
        
        #ADD RESET BUTTON
        shinyjs::show("reset_button2")
        
        ##SHIP ALL FILES AND METADATA
        
        #SHIP RAW FILE
        googledrive::drive_upload(media = paste0(prepared1$tmp.path, prepared1$name.me),
                                  path = rawdata_id,
                                  name = prepared1$name.me)
        
        #SHIP CLEAN FILE
        googledrive::drive_upload(media = paste0(tmp.path3, name.me3),
                                  path = cleandata_id,
                                  name = name.me3)
        
        #UPLOAD METADATA
        googlesheets4::sheet_append(data = metadata_df,
                                    ss = metadata_id,
                                    sheet = "submitted_metadata")
        
        #SHIP LOC FILE IF APPLICABLE
        if(all(isTruthy(input$location_file2),
           input$location_data2 == "No, my data file doesn't contain location data, but I can upload an additional file containing those location data.",
           prepared2 != "No location file")) {
          googledrive::drive_upload(media = paste0(prepared2$tmp.path, prepared2$name.me), #SHIP
                                    path = locdata_id,
                                    name = prepared2$name.me)
        }
        
      }
     }
    }

  })
 
  
# SECTION 5--OTHER OBSERVERS ----------------------------------------------
  
  ##OBSERVER FOR BACK BUTTON STATUS FACTORS, HANDLES LOCKING (NOT ADVANCEMENT)
  observeEvent(list(input$submission_tabset, sub_reactives$sub_complete), {
    
    if(input$submission_tabset != "1" &
       sub_reactives$sub_complete != TRUE) { 
      enable("new_sub_back")
    } else {
      disable("new_sub_back")
    }
    
  })
  
  ##OBSERVER FOR BACK BUTTON PRESSES, HANDLES ADVANCEMENT
  observeEvent(input$new_sub_back, {

    #FIND CURRENT TAB
    currTab = as.numeric(input$submission_tabset)
    
    #IF CURRENT TAB IS AFTER A BRANCHING LOGIC, CHECK PREVIOUS VALUES AND POSSIBLY REWIND TWO TABS
      if((currTab == 15 &&
          isTruthy(input$non_throws2) &&
          input$non_throws2 == "No non-throws") |
         (currTab == 17 &&
          isTruthy(input$rename_cols2) &&
          input$rename_cols2 == "Yes, looks good!") |
         (currTab == 19 &&
          isTruthy(input$location_data2) &&
          input$location_data2 != "No, my data file doesn't contain location data, but I can upload an additional file containing those location data.")) {
        
        nextTab = max(currTab-2, 1)
      } else {
        
        #IN ONE INSTANCE, PENDING CERTAIN RESPONSES, MAY SKIP 3 TABS BACK
        if(currTab == 15 &&
           input$acknowledge_data2 == "Yes, I understand; my current submission will not include any non-throws.") {
          nextTab = max(currTab-3, 1)
          updateTabsetPanel(session, "submission_tabset", 
                            selected = as.character(nextTab))
        } else {
        
        #OTHERWISE GO BACK JUST 1 TAB
        nextTab = max(currTab-1, 1)
        }
        
      }
    
    slideInL(currTab, nextTab, numQuestions)
    
    updateTabsetPanel(session, 
                      "submission_tabset", 
                      selected = as.character(nextTab))

  })
  
  
  ##OBSERVER FOR NEXT BUTTON STATUS TRACKER FROM VALIDATIONS, HANDLES LOCKING (NOT ADVANCEMENT)
  observeEvent(list(sub_reactives$isNextOpen, 
                    sub_reactives$sub_complete, 
                    sub_reactives$rattle_the_cage), { #INCLUDING THIS FORCES THIS TO RE-EVALUATE ON EVERY TRANSITION, EVEN IF WE'VE BACKED UP THRU QUESTIONS WE HAVE VALID ANSWERS TO AND isNextOpen IS STUCK AT "TRUE" BUT THE BUTTON REMAINS LOCKED JUST BECAUSE WE'VE HIT NEXT.
    
    if(sub_reactives$isNextOpen == TRUE && 
       sub_reactives$sub_complete != TRUE) {
      enable("new_sub_next")
    } else {
      disable("new_sub_next")
    }
    
  })
  
  #OBSERVER FOR RESET BUTTON, HANDLES RESETS
  observeEvent(input$reset_button2, ignoreInit = TRUE, {
    
    #RESET ALL REACTIVE VALUES
    sub_reactives$isNextOpen = FALSE 
    sub_reactives$sub_complete = FALSE
    sub_reactives$current_sub = NULL
    sub_reactives$current_path = NULL
    sub_reactives$max_start_row = Inf
    sub_reactives$max_end_row = Inf
    sub_reactives$COLUMNS_DELETE = NULL
    sub_reactives$COLUMNS_EMPTY = NULL
    sub_reactives$delete_message = NULL
    sub_reactives$delete_cols = NULL
    sub_reactives$ambiguous_cols = NULL
    sub_reactives$start_rows_cut_flag = FALSE
    sub_reactives$end_rows_cut_flag  = FALSE
    sub_reactives$sub_dfV1 = data.frame(matrix(NA, nrow=1))
    sub_reactives$sub_dfV2 = data.frame(matrix(NA, nrow=1))
    sub_reactives$sub_dfV3 = data.frame(matrix(NA, nrow=1))
    sub_reactives$sub_dfV4 = data.frame(matrix(NA, nrow=1))
    sub_reactives$sub_dfV5 = data.frame(matrix(NA, nrow=1))
    sub_reactives$sub_dfV6 = data.frame(matrix(NA, nrow=1))

    #RESET INPUTS AS APPROPRIATE
    reset("surveyors_names2")
    reset("lake_search")
    reset("lake_name2")
    reset("survey_date2")
    reset("depth_units2")
    reset("rake_units2")
    reset("data_file2")
    reset("start_row2")
    reset("end_row2")
    reset("flip_table2")
    reset("non_throws2")
    reset("non_throws_val2")
    reset("rename_cols2")
    reset("rename_comms2")
    reset("location_data2")
    reset("location_file2")
    reset("gen_comms2")
    
    #WIPE OUT ANY DTS AND MESSAGE UI ELEMENTS
    blankDT = DT::datatable(sub_reactives$sub_dfV1,
                            options = list(scrollX = TRUE, 
                                           searching = FALSE,
                                           fillContainer = T,
                                           language = list(
                                             search = "Search survey records: ",
                                             paginate = list(
                                               "next" = "Next table page",
                                               "previous" = "Previous table page"
                                             ))),
                            callback = DT_A11Y_Callback)
    output$sub_tableV1 <- renderDT({ blankDT })
    shinyjs::hide("sub_tableV1")
    output$sub_tableV2 <- renderDT({ blankDT })
    output$sub_tableV3 <- renderDT({ blankDT })
    output$sub_tableV4 <- renderDT({ blankDT })
    output$sub_tableV5 <- renderDT({ blankDT })
    output$sub_tableV6 <- renderDT({ blankDT })
    output$sub_tableV7 <- renderDT({ blankDT })
    output$sub_tableV8 <- renderDT({ blankDT })
    output$sub_tableV9 <- renderDT({ blankDT })
    output$sub_tableV10 <- renderDT({ blankDT })
    output$sub_tableV11 <- renderDT({ blankDT })
    output$file_type_warning <- renderUI({ })
    output$texty_columns2 <- renderText({ })
    output$columns_delete2a <- renderText({ })
    output$columns_delete2a <- renderText({ })
    output$loc_file_warning <- renderUI({ })
    output$confused_warning <- renderText({ })
    output$name_val_warning <- renderText({ })
    output$email_val_warning <- renderText({ })
    output$surveyors_val_warning <- renderText({ })
    output$date_val_warning <- renderText({ })
    output$depth_unit_warning <- renderText({ })
    output$rake_unit_warning <- renderText({ })
    output$flip_table_warning <- renderText({ })
    output$column_name_warning <- renderText({ })
    output$row_start_warning <- renderText({ })
    output$row_end_warning <- renderText({ })
    
    #REMOVE 'SUBMISSION COMPLETE' NOTE
    shiny::removeNotification("submitted_note2")
    
    #ADVANCE TO PROPER TAB
    updateTabsetPanel(session, "submission_tabset", selected = "2")
    
    #HIDE POST-SUBMISSION BUTTONS
    shinyjs::hide("reset_button2")
    shinyjs::hide("download_now2")
    
    #REMOVE LOC FILE POINTER, IF APPLICABLE (OTHERWISE, COULD PERSIST AND GUM UP NEXT SUBMISSION AFTER RESET)
    if(isTruthy(prepared2)) {
      prepared2 <<- NULL
    }
 
  })
  

  #OBSERVER FOR POPULATING LAKE/DOW SELECTOR BASED ON THE SEARCH BAR INPUTS
  observe({  

    search_term <- search_input() #SEARCH_INPUT IS A DEBOUNCED REACTIVE VAL
  
    #ONLY UPDATE INPUT IF SEARCH TERM > LENGTH 2 AND ANY RESULTS PRODUCED
    if (isTruthy(search_term) && 
        nchar(search_term) >= 3 && 
      length(grepl(search_term, lakedownames, fixed=T)) > 0) {
      
      #FILTER FROM OUR COMPLETE LIST OF OPTIONS
      filtered_lakes <- lake.dows[grepl(search_term, lakedownames, fixed=T)]
      updateSelectizeInput(session, "lake_name2", 
                           choices = c("Select a lake", filtered_lakes), 
                           options = list(maxOptions = 2000),
                           server = TRUE)
    }
  })
  
  #OBSERVER WATCHING SUBMITTED FILES, UPDATES FILE TRACKER
  shiny::observeEvent(input$data_file2, {
    req(isTruthy(input$data_file2)) #USEFUL REQ()
    sub_reactives$current_sub <- input$data_file2
  })
  
  #OBSERVER WATCHING FILE TRACKER, UPDATES PATH TRACKER
  shiny::observeEvent(sub_reactives$current_sub, {
    req(isTruthy(sub_reactives$current_sub))
    tempfilepath = sub_reactives$current_sub
    sub_reactives$current_path <- tempfilepath$datapath
  })
  
  #OBSERVER WATCHING FILE PATH, ATTEMPTS TO READ FILE AS TABLE DEPENDING ON FILE EXTENTION.
  observeEvent(sub_reactives$current_path, {
    req(isTruthy(sub_reactives$current_path))

    current.filepath = sub_reactives$current_path #POINTER
    
    #CSV READ
    if(grepl(".csv", current.filepath, ignore.case = T)) {
      
      prelim_df = utils::read.csv(current.filepath, 
                                  header = TRUE, 
                                  strip.white = TRUE, 
                                  na.strings = c("", "NA"), 
                                  blank.lines.skip = TRUE)
      
      prelim_df = initial.dfprocessing(prelim_df) #PROCESS
      sub_reactives$sub_dfV1 <- prelim_df #STORE AS REACTIVE
      
    } else {
      
      #EXCEL READ
      if(grepl(".xls", current.filepath, ignore.case = T)) {

          prelim_df = readxl::read_excel(current.filepath, 
                                         trim_ws = T, 
                                         col_names = TRUE) 
          
          prelim_df = initial.dfprocessing(prelim_df) #PROCESS
          sub_reactives$sub_dfV1 <- prelim_df #STORE

      } else {
        
        #TSV READ
        if(grepl(".tsv", current.filepath, ignore.case = T)) {
          
          prelim_df = utils::read.table(current.filepath, 
                                        header = TRUE, 
                                        strip.white = TRUE, 
                                        na.strings = c("", "NA"), 
                                        blank.lines.skip = TRUE,
                                        sep = "\t")
          
          prelim_df = initial.dfprocessing(prelim_df) #PROCESS
          sub_reactives$sub_dfV1 <- prelim_df #STORE
          
        } else {
          
          #SHP FILE READ. MUST BE A ZIP CONTAINING SHP FILE.
          if(grepl(".zip", current.filepath, ignore.case = T)) {
            
            tmp.path_zip = file.path(paste0(tempdir(), "\\")) #MAKE TEMP DIR
            utils::unzip(current.filepath, exdir = tmp.path_zip) #UNZIP INTO
            
            #CHECK FOR SHP FILE
            if(isTruthy(grepl(".shp", list.files(tmp.path_zip)))) {
              
              zip_filepath = list.files(tmp.path_zip)[grepl(".shp", list.files(tmp.path_zip))] #FIND ANY SHP FILE.
              
              prelim_df = sf::st_read(paste0(tmp.path_zip, zip_filepath)) #READ
              
              names(prelim_df) = trimws(names(prelim_df)) #PROCESS
              prelim_df = sf::st_drop_geometry(prelim_df)
              prelim_df = initial.dfprocessing(prelim_df)
              
              sub_reactives$sub_dfV1 <- prelim_df #STORE
              
            } else {
              sub_reactives$isNextOpen <- FALSE #If no shape file is included, need to shut this down. 
            }
          }
        }
      }
    }
  })
  

#OBSERVER WATCHING READ FILE, DISPLAYS FIRST TABLE
  observeEvent(sub_reactives$sub_dfV1, ignoreInit = TRUE, {

  if(isTruthy(sub_reactives$current_path)) { #SHOW MESSAGE BUT NOT ON RESET
  shinyjs::show("sub_tableV1")
  }

  #SHOW TABLE
  output$sub_tableV1 <- DT::renderDT({
    DT::datatable(sub_reactives$sub_dfV1,
                  selection = 'none',
                  options = list(scrollX = TRUE,
                                 searching = FALSE,
                                       fillContainer = T,
                                 language = list(
                                   search = "Search survey records: ",
                                   paginate = list(
                                     "next" = "Next table page",
                                     "previous" = "Previous table page"
                                   ))),
                  callback = DT_A11Y_Callback)
  })

})

#OBSERVER FOR SHOW/HIDE INSTRUCTIONS BUTTON
observeEvent(input$new_sub_showhide, {
  
  #IF ODD, HIDE INSTRUCTION (STARTS OPEN)
  if(!checkIfEven(input$new_sub_showhide)) {
    
    shinyjs::hide("new_sub_instruct",
         anim=T,
         animType = "slide")
    
    updateActionButton(session, 
                       "new_sub_showhide", 
                       label = "Show instructions")
    
    runjs(paste0("$('#new_sub_showhide').attr('aria-expanded', 'false');"))
    
  } else {
    #IF EVEN, SHOW INSTRUCTION
    shinyjs::show("new_sub_instruct",
         anim=T,
         animType = "slide")
    
    updateActionButton(session, 
                       "new_sub_showhide", 
                       label = "Hide instructions")
    
    runjs(paste0("$('#new_sub_showhide').attr('aria-expanded', 'true');"))
    
  }
  
})
  
}