submissionsUI_V2 = function() {
  
  shiny::tabPanel(value = 3,
    shiny::div(shiny::span(shiny::HTML('<span class="fa-solid fa-file-import" aria-hidden="true"></span>'), 
                                         "Submissions")),
                  class = "submissionTab",
    
      tags$main( #SEMANTICALLY APPROPRIATE DIV
    
    #SOME JS CODE THAT WILL FIND THE INPUT OR SELECT ELEMENTS ASSOCIATED WITH SOME OF THE LABELS OF SUBMISSIONS TAB QUESTIONS AND PROPERLY ASSOCIATE THEM SO ASSISTIVE TECHNOLOGY KNOWS THE LABEL FOR THIS INPUT GOES WITH THE INPUT. 
    
    #THE FIRST ONE HERE FINDS THE INPUT ASSOCIATED WITH THE DATE PICKER AND GIVES IT AN ID AND THEN ADJUSTS THE LABEL'S FOR ATTRIBUTE TO MATCH THAT ID.
    #THE SECOND ONE HERE JUST FINDS THE LAKE_NAME INPUT'S LABEL AND GIVES IT THE PROPER FOR ATTRIBUTE. HOWEVER, IT HAS TO WAIT UNTIL SELECTIZE.JS RUNS OR IT'LL GET OVERRIDDEN.
    #THE THIRD ONE HERE FINDS THE TEXT INPUT THAT HOLDS THE SUBMITTED FILE'S NAME FOR THE FILE INPUT AND GIVES THAT INPUT AN ARIA-LABEL SO IT'S NOT UNLABELED.
    #THE FOURTH DOES THE SAME BUT FOR THE LOCATION FILE INPUT.
    #THE LAST TWO MAKE SURE TWO OF THE SELECTIZE INPUTS HAVE LOADED AND FORCES THEM TO HAVE ARIA-LABELS SINCE THE REAL LABELS ARE ASSOCIATED WITH OTHER ELEMENTS.
    tags$script(HTML("
    $(document).ready(function() {
      var inputEl = $('#survey_date2').find('input').first();
      if (inputEl.length) {
        inputEl.attr('id', 'survey_date2_input');
        $('#survey_date2-label').attr('for', 'survey_date2_input');
      }
    });
    
$(document).ready(function() {
  var updateLabelFor = function() {
    var lakeLab = document.getElementById('lake_name2-label');
    if (lakeLab) {
      lakeLab.setAttribute('for', 'lake_name2');
    }
    var lakeInput = document.getElementById('lake_name2-selectized');
    if(lakeInput) {
    lakeInput.setAttribute('aria-label', 'Select a lake from this list.');
    }
   };

  // Check if selectize has initialized
  if (document.getElementById('lake_name2-selectized')) {
    updateLabelFor(); // If already initialized, update immediately
  } else {
    // Wait for selectize to initialize
    var interval = setInterval(function() {
      if (document.getElementById('lake_name2-selectized')) {
        updateLabelFor();
        clearInterval(interval); // Stop checking once updated
      }
    }, 100); // Check every 100 milliseconds
  }
});

    $(document).ready(function() {
    var textInput = $('#data_file2').closest('.form-group').find('input[type=\"text\"]');
    if (textInput.length > 0) {
      textInput.attr('aria-label', 'Selected file name');
    }
  });
  
      $(document).ready(function() {
    var textInput = $('#location_file2').closest('.form-group').find('input[type=\"text\"]');
    if (textInput.length > 0) {
      textInput.attr('aria-label', 'Selected file name');
    }
  });

  $(document).ready(function() {
    var checkExist = setInterval(function() {
    var selectEl = document.getElementById('non_throws2');
    if (selectEl) {
      selectEl.setAttribute('aria-label', 'Select an option from the menu.');
      clearInterval(checkExist); 
    }}, 100);
  });
  
  $(document).ready(function() {
    var checkExist = setInterval(function() {
    var selectEl = document.getElementById('non_throws_val2');
    if (selectEl) {
      selectEl.setAttribute('aria-label', 'Select an option from the menu.');
      clearInterval(checkExist); 
    }}, 100);
  });
  
  //ADD AUTOCOMPLETE ATTRIBUTES TO THESE ELEMENTS
  $(document).ready(function() {
      $('#submitter_name2').attr('autocomplete', 'name');
      $('#submitter_email2').attr('autocomplete', 'email');
    });

  ")),
    
                  ## Submissions tab, element 1 (INSTRUCTIONS) ---------------------------------------------
                  
    ##SHOW/HIDE INSTRUCTIONS BUTTON
    shiny::fluidRow(id = "sub_instruct_row",
                    tags$h3("Submissions instructions menu", class = "sr-only"), #SCREEN READER HEADING.
                    shiny::column(width = 2, offset = 5, 
                                  id = "sub_instruct_cell",
                                  actionButton("new_sub_showhide", 
                                               "Hide instructions", 
                                               class = "mapInputs browse_buttons",
                                               `aria-expanded` = "true",
                                               `aria-controls` = "new_sub_instruct"),
                                  align = "center")),

    ##INSTRUCTIONS PANEL
                  shiny::div(
                    id = "new_sub_instruct", 
                             shiny::HTML("<h4>How Do I Use This Tab?</h4>Use this tab to submit new P.I. survey files to <a href ='https://maisrc.umn.edu/'>MAISRC</a>'s database! Below, the app will ask you a set of questions. As you answer, it will display--and even possibly reformat--your data to ensure your data are consistent with our database's requirements. This will enable the app to do most of the processing that MAISRC staff would otherwise have to do 'by hand' to unify your submissions with those of all others we've received. <strong>A more detailed, step-by-step <a href='https://www.youtube.com/watch?v=iu8jS4cwF-g', target='_blank'>tutorial for the usage of the Submissions and Leaderboard tabs</a> of P.I. Charter can be viewed on MAISRC\'s Youtube channel (closed captioning available; link opens in a new tab). If you are not sure if what you have is a P.I. survey, consult the 'How are P.I. Surveys Different From Other Kinds?' section of the help menu on the 'Overview' tab.</strong><br><br>
         
         Other usage notes:<br><ol>
         <li>There is no way the app can save your progress on a submission so that you can continue it later. Please plan to complete each submission in a single session.</li> 
                             <li>Avoid submitting Excel files containing merged cells; merging is not a feature R interprets correctly.</li>
                             <li>It is not possible to change specific values inside your data set while using this tab. If you spot an error in your data, fix it in your original file, then restart the submission process. </li>
                             <li>Note that submitted files will not typically enter our database immediately because they have to first be checked over by MAISRC staff. See the Disclaimers section on the Overview tab for details.</li></ol>")),
                  
    tags$form( #SEMANTICALLY APPROPRIATE DIV
      tags$h3("Submissions form", class = "sr-only"), #SCREEN READER HEADING.
      `aria-describedby` = "progress_bar_text", #TO MAKE THE LINK TO THE PROGRESS BAR MORE OBVIOUS.
      tags$div(
        role = "status", #MORE SEMANTIC REINFORCEMENT AS TO WHAT THIS ALL IS DOING.
    uiOutput("progress_bar_text"),
    tags$h4("Submissions progress bar", class = "sr-only"), #SCREEN READER HEADING.
    ), #ADD VOICED UI ELEMENT HERE TO INDICATE THIS IS A PROGRESS BAR.
    
    ##START SUBMISSION TABSET PANEL
    tabsetPanel(id = "submission_tabset", 
                type = "pills",
                footer = div(id = "next_back_div",

                tags$h4("Submissions question navigation menu", class = "sr-only"), #SCREEN READER HEADING.
                             
                  ##NEXT AND BACK BUTTONS FOR TAB ADVANCEMENT
                  actionButton("new_sub_back", "Back", 
                               class = "mapInputs nextback"),
                  actionButton("new_sub_next", "Next", 
                               class = "mapInputs nextback")
                ),
      
      #ABSENCES VS NON-THROWS QUESTION (MANUAL HTML CONSTRUCTION)
      tabPanel(title = 1, value = 1, 
               
               shiny::div(class = "absences_nonthrows",
                          shiny::HTML("<h3 class = 'flexthis justifycenter'>Start here: Question #1</h3>
                          <p class = 'centertext'><b>PI survey data fall into three 'types.' We need only some of them...</b></p>
                                                 <table class = absences_nonthrows>
                                                  <tr>
                                                    <th>Your plan: Sample 100 sites in a lake…</th>
                                                    <th>We’d call these:</th>
                                                    <th class = 'centertext'>Do we want 'em?</th>
                                                  </tr>
                                                  <tr>
                                                    <td>You find 1+ taxa at 50 sites.</td>
                                                    <td>“Presences”</td>
                                                    <td class = greenme>Yes! These are critical!</td>
                                                  </tr>
                                                  <tr>
                                                    <td>You find no plants at 30 sites</td>
                                                    <td>“Absences”</td>
                                                    <td class = greenme><strong>Yes! Where plants <em>aren’t</em> is as important as where they <em>are</em>! Leave these “empty” rows, or mark them in a “no plants found” column.</strong></td>
                                                  </tr>
                                                 <tr>
                                                    <td>You don’t sample the other 20 sites.</td>
                                                    <td>“Non-throws”</td>
                                                    <td class = redme><strong>No! You can remove these rows from your data sheets; alternatively, the app will help you remove them during submission (if it can).</strong></td>
                                                  </tr>
                                                 </table>")),
               div(class = "new_sub_divs",
                   ##ACKNOWLEDGMENT QUESTION
                 shiny::radioButtons("acknowledge_data2", 
                                    width = "100%",
                                  label = "Do you understand the distinction between 'non-throws' and 'absences' and can confirm you're submitting all the data we're looking for?", 
                                  choices = c("No selection", "Yes, I understand; my current submission will not include any non-throws.", "Yes, I understand; my current submission will include non-throws, and I will need the app to help me eliminate them.", "No, I'm confused by this."),
                                  selected = "No selection" #IT'S GOOD FOR ACCESSIBILITY AND ROBUSTNESS TO BE EXTRA EXPLICIT ABOUT THE STARTING VALUE.
                                  ),
                 #USER FEEDBACK IF CONFUSED
                 tags$div(
                   `aria-live` = "polite",
                   `aria-atomic` = "true",
                   hidden(htmlOutput("confused_warning")
                 ))
      )),
      
      #NAME QUESTION
      tabPanel(title = 2, value = 2,
             
      div(class = "new_sub_divs",
          textInput("submitter_name2",
                label = shiny::HTML('What is your full, capitalized name (e.g., Alex Bajcz) or that of your organization? (As it\'ll be displayed on the "Leaderboard" tab!)'),
                placeholder = "Betsy Submitter",
                value = "",
                width = "100%"),
          tags$div(
            `aria-live` = "polite",
            `aria-atomic` = "true",
            htmlOutput("name_val_warning") #VALIDATION MESSAGE
          )
      )),
      
      #EMAIL QUESTION
      tabPanel(title = 3, value = 3,
      
      div(class = "new_sub_divs",
          textInput("submitter_email2",
                shiny::HTML("What is your email address (for if we have questions)? Use Me@address.ending format."),
                value = "",
                width = "100%"),
          tags$div(
            `aria-live` = "polite",
            `aria-atomic` = "true",
            htmlOutput("email_val_warning") #VALIDATION MESSAGE
          )
      )),
      
      #SURVEYORS QUESTION
      tabPanel(title = 4, value = 4,
      
      div(class = "new_sub_divs",
        textInput(inputId = "surveyors_names2",
                label = shiny::HTML('Who was/were the surveyor(s) on the survey you\'re submitting? Use commas to separate names and "NA" for anonymous surveyors, e.g., "Scott Surveyor, Megan Plant-Expert, NA". (As they\'ll be displayed on the "Leaderboard" tab!)'), 
                value = "",
                width = "100%"
               ),
        tags$div(
          `aria-live` = "polite",
          `aria-atomic` = "true",
          htmlOutput("surveyors_val_warning") #VALIDATION MESSAGE
        )
      )),
      
      #LAKE NAME/DOW QUESTION
      tabPanel(title = 5, value = 5,
             
      tags$fieldset(class = "new_sub_divs add50height", 
          tags$legend(HTML("Which lake was surveyed? Start typing a lake name or 8-digit DOW number in the first search box to search our database of lakes, then select your lake from the dropdown menu. Not sure of your lake's name or DOW number? <a target= '_blank', href='https://maps1.dnr.state.mn.us/lakefinder/mobile/'>Use LakeFinder to find it!</a>"),
                      style = "font-size: 95%; font-weight: bold;"),
          #SEARCH BAR TO HANDLE SERVER-SIDE CHOICE PROCESSING FOR SELECTOR BELOW
          textInput("lake_search", 
                    label = HTML("Search for your lake (<strong>Note: at least three characters are needed!</strong>)."),
                    width = "100%",
                    value = ""),
          #ACTUAL LAKE SELECTOR
          selectizeInput("lake_name2", 
                label = shiny::HTML("Select your lake (if no options are available, you\'ve entered an invalid search string in the search box). This question is required; the \'Next\' button below this question will remain locked until you have selected a lake here."),
                width = "100%",
                choices = NULL)
        
      )),
      
      #SURVEY START DATE QUESTION
      tabPanel(title = 6, value = 6, 
            
      div(class = "new_sub_divs",
          dateInput("survey_date2",
                shiny::HTML("On what date did the survey start? <strong>Important: Use mm-dd-yyyy format!</strong> This is a required question; the \'Next\' button below this question will remain locked until a plausible survey date is entered."),
                format = "mm-dd-yyyy", 
                width = "100%",
                value = Sys.Date() - 1), #SET TO YESTERDAY'S DATE.
          tags$div(
            `aria-live` = "polite",
            `aria-atomic` = "true",
            htmlOutput("date_val_warning") #VALIDATION MESSAGE
          )
      )),
      
      #DEPTH UNITS QUESTION
      tabPanel(title = 7, value = 7,
      
      div(class = "new_sub_divs",
          selectInput("depth_units2",
                  shiny::HTML("What were the units for any water depth column(s) in your data set? If there is no depth column in your data set, select 'None.'<br><br>FYI: Later, the app will automatically convert your depth data (if any) into feet, if they aren't already, so don\'t be surprised when this happens!"),
                  choices = c("No selection", "Feet", "Meters", "None", "The data set has a column each for depths in meters and in feet"),
                  selected = "No selection",
                  width = "100%",
                  size = 6,
                  selectize = F), 
          tags$div(
            `aria-live` = "polite",
            `aria-atomic` = "true",
            htmlOutput("depth_unit_warning") #VALIDATION MESSAGE     
          )
      )),
      
      #MAX RAKE SCORE QUESTION
      tabPanel(title = 8, value = 8,
       
      div(class = "new_sub_divs",
          selectInput("rake_units2",
                  "What was the HIGHEST value the rake data in your survey could have gone up to? This is typically a number between 1 and 5. If your data are 'presence/absence,' select 1 here.",
                  choices = c("No selection", 1, 2, 3, 4, 5, "Wet weights (g) instead of rake scores", "Unknown", "Something else (explain later please!)"),
                  selected = "No selection",
                  width = "100%",
                  size = 9,
                  selectize = F
               ), 
          tags$div(
            `aria-live` = "polite",
            `aria-atomic` = "true",
            htmlOutput("rake_unit_warning") #VALIDATION MESSAGE  
          )
      )),
      
      #UPLOAD FILE QUESTION
      tabPanel(title = 9, value = 9,
      div(class = "sub_2pane_div", 
          id = "sub_2pane_box",
          #START DIVIDING INTO TWO HALVES, W/ QUESTIONS ON LEFT.
      div(class = "new_sub_divs_halfL",
          fileInput("data_file2",
                shiny::HTML("Next, use the file picker to upload your survey data file. <strong>Important: This should be a single, raw, and tabular data sheet, such as a single Excel, CSV, or TSV spreadsheet.</strong><br><br>If your data are in a shape file (.shp), zip together ALL files for a given lake (the .shp file plus all its companion files, such as .prj, .sbx, and .shx files) and submit the zip file here. If your data are in a Gsheet, please download the Gsheet as a CSV file and submit that file here. This is a required question; the \'Next\' button below this question will not unlock until a valid survey file has been uploaded."),
                width = "100%"),
          tags$div(
            `aria-live` = "polite",
            `aria-atomic` = "true",
            uiOutput("file_type_warning")
          )
          ),

      #RIGHT-HAND SIDE: NEWLY SUBMITTED TABLE.
          div(class = "new_sub_divs_halfR", 
              hidden(dataTableOutput("sub_tableV1")))       
        )
       ),
      
      #START ROW QUESTION
      tabPanel(title = 10, value = 10, 
      
      div(class = "sub_2pane_div", 
      div(class = "new_sub_divs_halfL",
          numericInput("start_row2",
                   shiny::HTML("Because we want only your raw data and no summaries, comments, blank rows, etc., we may need to 'cut down' your submitted data file. Indicate below the number of the row in which your PI Survey data START. Select EITHER row 1 (if your data start there) OR the row that holds your 'header row' of column names. Use the following table of your data for reference. NOTE: If your column names are in row 1 in the table shown instead of at the top of the table, please open up your data file, delete row 1, then begin this submission again."), #***Awkward!
                 value = 1, 
                 min = 1,
                 max = 10, 
                 step = 1,
                 width="100%"
               ),
          tags$div(
            `aria-live` = "polite",
            `aria-atomic` = "true",
            htmlOutput("row_start_warning") #VALIDATION MESSAGE
          )
          ),
      div(class = "new_sub_divs_halfR",
          dataTableOutput("sub_tableV2") #UNALTERED TABLE
      )
      )),
      
      #END ROW QUESTION
      tabPanel(title = 11, value = 11,
      
      div(class = "sub_2pane_div",   
      div(class = "new_sub_divs_halfL",
        numericInput("end_row2",
                   shiny::HTML("Indicate the row number where your raw PI survey data end. Exclude rows containing summaries, incomplete data,comments, etc. Use the table to the right for reference."),
                   value = 10, 
                   min = 2,
                   max = 10, 
                   step = 1,
                   width = "100%"),
        tags$div(
          `aria-live` = "polite",
          `aria-atomic` = "true",
          htmlOutput("row_end_warning") #VALIDATION MESSAGE
        )
      ),
      div(class = "new_sub_divs_halfR",
          dataTableOutput("sub_tableV3") #TABLE CLIPPED TO START ROW
       )
      )),
      
      #FLIP TABLE QUESTION
      tabPanel(title = 12, value = 12, 
      
      div(class = "sub_2pane_div",   
      div(class = "new_sub_divs_halfL",
          radioButtons("flip_table2",
                  shiny::HTML("To add your data to our database, we will need your rows to represent individual survey points in the lake and your columns to be data from each point (e.g., depth, taxon 1, taxon 2, etc.). Indicate whether we must flip your rows and columns to achieve our desired configuration."),
                  choices = c("No selection", "No, don't flip my table!", "Yes, flip my table!"), 
                  width = "100%",
                  selected = "No selection"), 
          tags$div(
            `aria-live` = "polite",
            `aria-atomic` = "true",
            htmlOutput("flip_table_warning") #VALIDATION MESSAGE        
          )
      ),
      div(class = "new_sub_divs_halfR",
          dataTableOutput("sub_tableV4") #TABLE CLIPPED TO END ROW
        )
      )),
      
      #NON-THROWS QUESTION
      tabPanel(title = 13, value = 13,
      
      div(class = "sub_2pane_div", 
      div(class = "new_sub_divs_halfL",
          selectInput("non_throws2",
                shiny::HTML("Sometimes, locations are not actually sampled (e.g., they were too deep). <strong>Note: If your table DOES NOT contain any rows for these 'non-throws,' skip this question by choosing 'No non-throws' from the drop-down below.</strong><br><br>However, if your table does contain rows for 'non-throws,' we need to eliminate these. Select the NAME of the column where non-throws are marked. For example, if you record 'non-throws' by writing 'too deep' in your depth column, select the depth column here. This is a required question; the \'Next'\ button below this question will not unlock until a valid choice is made."),
                selected = "No selection",
                choices = c("No selection", "No non-throws"),
                width = "100%")
      ),
      div(class = "new_sub_divs_halfR",
          dataTableOutput("sub_tableV5") #FLIPPED TABLE
      )
      )),
      
      #NON-THROWS VALUES QUESTION
      tabPanel(title = 14, value = 14, 
      
      div(class = "sub_2pane_div",
      div(class = "new_sub_divs_halfL",
                  shinyWidgets::pickerInput("non_throws_val2",
                  "Ok, we can skip this question then!",
                  choices = "Proceed to the next question!",
                  selected = "Proceed to the next question!",
                  multiple = TRUE,
                  width = "100%")
      ),
      div(class = "new_sub_divs_halfR",
          dataTableOutput("sub_tableV6") #STILL FLIPPED TABLE
      )
      )),
      
      #COLUMN NAMES ISSUES QUESTION
      tabPanel(title = 15, value = 15,
      
      div(class = "sub_2pane_div",
      div(class = "new_sub_divs_halfL",
          radioButtons("rename_cols2", #SUPPOSEDLY, INSTEAD OF DOING SELECTIZE =F, SIZE = X FOR 5 OR FEWER OPTIONS, RADIOBUTTONS IS SMOOTHER AND MORE ACCESSIBLE (SHRUGS)
                  shiny::HTML("We must ensure your columns have names consistent with those in our database. In the table to the right, your 'old' column names are shown at the top of the table. The 'new' column names we're going to change them to are now shown in the first row. Please compare these sets of names and double-check that our new column names are appropriate.<br><br><strong>Important: Pay special attention to any columns marked 'delete' because these will be deleted by the app prior to submission as a result of an unrecognized column name. Also pay attention to any marked as 'ambiguous', as these will need clarifying to be retainable.</strong>"),
                  choices = c("No selection", "Yes, looks good!", "No, there are some issues..."),
                  width = "100%", 
                  selected = "No selection"),
          tags$div(
            `aria-live` = "polite",
            `aria-atomic` = "true",
            htmlOutput("column_name_warning")
          ),
          tags$div(
            `aria-live` = "polite",
            `aria-atomic` = "true",
            htmlOutput("columns_delete2a") #USER-AIMED WARNING: AMBIGUOUS/UNRECOGNIZED COLUMNS
          )
      ),
      div(class = "new_sub_divs_halfR",
          dataTableOutput("sub_tableV7") #NON-THROWS REMOVED TABLE, AMBIG/DELETE COLUMNS HIGHLIGHTED
      )
      )),
      
      #COLUMN NAMES COMMENTS QUESTION
      tabPanel(title = 16, value = 16,
      
      div(class = "sub_2pane_div",
      div(class = "new_sub_divs_halfL",
          textAreaInput("rename_comms2",
                    "You indicated there were issues with our new column names. Tell us what you noticed. We'll fix these issues after your data are submitted.",
                    resize = "vertical", 
                    value = "",
                    placeholder = "Your comments here...",
                    width = "100%"),
          tags$div(
            `aria-live` = "polite",
            `aria-atomic` = "true",
            htmlOutput("column_val_warning")
          ),
          tags$div(
            `aria-live` = "polite",
            `aria-atomic` = "true",
            htmlOutput("columns_delete2b") #USER-AIMED WARNING: AMBIGUOUS/UNRECOGNIZED COLUMNS
          )
      ),
      div(class = "new_sub_divs_halfR",
          dataTableOutput("sub_tableV8") #NON-THROWS REMOVED TABLE, AMBIG/DELETE COLUMNS HIGHLIGHTED
      )
      )),
      
      #LOCATION DATA QUESTION
      tabPanel(title = 17, value = 17, 
      
      div(class = "sub_2pane_div",
      div(class = "new_sub_divs_halfL",
          selectInput("location_data2",
                  shiny::HTML("Indicate below whether your data file (the one displayed to the right) includes data on the spatial locations of your sample points. If not, indicate whether you have an additional file with location data you could upload. If so, the next question will allow you to do so (though it is our <strong>strong</strong> preference that those data be included directly in your data file). Otherwise, indicate that you do not have (or will not be sharing) location data for this survey."),
                  choices = c("No selection",
                                       "Yes, my raw data file contains UTM data for every point.",
                                       "Yes, my raw data file contains lat-long data for every point.",
                                       "Yes, my raw data file contains both lat-long and UTM data for every point.",
                                       "No, my data file doesn't contain location data, but I can upload an additional file containing those location data.",
                                       "No, I can't share location data or don't have them."),
                  width = "100%",
                  selected = "No selection",
                  selectize = F,
                  size = 6),
          tags$div(
            `aria-live` = "polite",
            `aria-atomic` = "true",
            htmlOutput("utm_warning") #USER-AIMED WARNING: UTM data
          )
      ),
      div(class = "new_sub_divs_halfR",
          dataTableOutput("sub_tableV9") #COLUMNS RENAMED TABLE
      )
      )),
      
      #LOCATION FILE QUESTION
      tabPanel(title = 18, value = 18, 
      
      div(class = "sub_2pane_div",
      div(class = "new_sub_divs_halfL flex_div",
          fileInput("location_file2",
                shiny::HTML("You indicated you have location data in a separate file. Upload that file here. If that file is a .shp file, zip the .shp file and all its companion files into a single .zip file and submit the .zip file here. This is a required question; the \'Next\' button below this question will not unlock until a selection is made."),
                width = "100%"),
          tags$div(
            `aria-live` = "polite",
            `aria-atomic` = "true",
            uiOutput("loc_file_warning")
          )
      ),
      div(class = "new_sub_divs_halfR",
          dataTableOutput("sub_tableV10") #COLUMNS RENAMED TABLE
      )
      )),
      
      #COMMENTS QUESTION
      tabPanel(title = 19, value = 19,
      
      div(class = "sub_2pane_div",
      div(class = "new_sub_divs_halfL flex_div",
          textAreaInput("gen_comms2",
                    shiny::HTML("You\'re ready to submit your data! Any comments? Note: We\'d especially appreciate feedback on this app!"),
                    resize = "vertical",
                    placeholder = "Tell us anything else you think we need to know!",
                    width = "100%"
               ),
          #DOWNLOAD CLEAN FILE BUTTON UPON SUBMISSION
          shinyjs::hidden(shiny::downloadButton("download_now2",
                                shiny::span("Download the processed data."),
                                class = "mapInputs")),
          #RESET BUTTON UPON SUBMISSION
          shinyjs::hidden(shiny::actionButton("reset_button2", 
                                              shiny::span(shiny::HTML('<span class="fa-solid fa-redo" aria-hidden = "true"></span>'), "Click here to submit another file!"), 
                                              class = "mapInputs",
                                              width = "100%")
      ),
      
      #WARNING--COLUMNS CONTAINING TEXT VALS
      hidden(htmlOutput("texty_columns2")),
      ),
      div(class = "new_sub_divs_halfR",
          dataTableOutput("sub_tableV11") #TIDIED TABLE
       )))
     )
    ),
    
    #ENSURE THAT SCREEN READERS KNOW THE TABS ON THIS PAGE ARE PURELY DECORATIVE AND SHOULDN'T GET FOCUS. NEEDS TO RUN AFTER THE ITEM HAS BEEN BUILT SO THAT'S WHY IT'S DOWN HERE??
    tags$script(HTML("
  $(document).ready(function() {
    var checkExist = setInterval(function() {
      var $liItems = $('#submission_tabset li');
      var $links = $('#submission_tabset li a');

      // Ensure we have both li and a elements before proceeding
      if ($liItems.length > 0 && $links.length > 0) {
        // Modify li elements
        $liItems.each(function() {
          $(this)
            .attr('tabindex', '-1')
            .attr('role', 'presentation')
            .attr('aria-disabled', 'true');
        });

        // Modify a elements
        $links.each(function() {
          $(this)
            .attr('tabindex', '-1')
            .attr('role', 'presentation')
            .attr('aria-disabled', 'true')
            .removeAttr('data-toggle')
            .removeAttr('aria-controls')
            .removeAttr('aria-selected')
            .removeAttr('aria-expanded');
            ;
            
        });

        clearInterval(checkExist);
      }
    }, 100);
  });
"))
   )
 )
  
}