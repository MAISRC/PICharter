recordsUI = function() {

  #ESTABLISH TAB PANEL
  shiny::tabPanel(value = 4,
 
      #HAVE A TAB TITLE PANEL AS ON OTHER TABS
       shiny::div(shiny::span(shiny::HTML('<span class="fa-solid fa-clipboard" aria-hidden="true"></span>'), "Records")),
      #SINGLE FLUID PAGE
       shiny::fluidPage(
           style = "padding-left: 0px; padding-right: 0px; display: flex; flex-wrap: wrap;",
           id = "records_page",
         
         #THIS JS MAKES SURE THE SELECTIZE INPUTS HAVE LOADED AND FORCES THEM TO HAVE ARIA-LABELS SINCE SELECTIZE INPUTS HAVE TWO INPUTS (THE MENU AND THE SEARCH BOX) BUT ONLY GET ONE LABEL, SO WE HAVE TO CREATE A SECOND LABEL FOR THE OTHER FOR SCREEN READERS.
         tags$head(
           tags$script(HTML("
         $(document).ready(function() {
           var checkExist = setInterval(function() {
             var selectEl = document.getElementById('records_dows');
             if (selectEl) {
               selectEl.setAttribute('aria-label', 'Select a lake from the menu.');
               // Now find the dropdown content element created by selectize
      var dropdown = document.querySelector('.selectize-dropdown-content[role=\"listbox\"]');
      if (dropdown) {
        dropdown.setAttribute('aria-label', 'List of available options');
      }
               clearInterval(checkExist); 
             }}, 100);
         });
        $(document).ready(function() {
           var checkExist = setInterval(function() {
             var selectEl = document.getElementById('abundance_survey');
             if (selectEl) {
               selectEl.setAttribute('aria-label', 'Select a survey from the menu.');
               // Now find the dropdown content element created by selectize
      var dropdown = document.querySelector('.selectize-dropdown-content[role=\"listbox\"]');
      if (dropdown) {
        dropdown.setAttribute('aria-label', 'List of available options');
      }
               clearInterval(checkExist); 
             }}, 100);
         });
         $(document).ready(function() {
           var checkExist = setInterval(function() {
             var selectEl = document.getElementById('abundance_taxon');
             if (selectEl) {
               selectEl.setAttribute('aria-label', 'Select a taxon from the menu.');
               // Now find the dropdown content element created by selectize
      var dropdown = document.querySelector('.selectize-dropdown-content[role=\"listbox\"]');
      if (dropdown) {
        dropdown.setAttribute('aria-label', 'List of available options');
      }
               clearInterval(checkExist); 
             }}, 100);
         });
         "))),

       #TWO COLUMNS--LEFT-HAND SIDE PANEL
       column(width = 3, 
            tags$aside( #SEMANTICALLY APPROPRIATE DIV
              tags$h3("Survey records selection menu", class = "sr-only"), #SCREEN READER HEADING.
              tags$a(href = "https://www.youtube.com/watch?v=aX5mOzsezwE", 
                     target = "_blank",
                     "Click here to view a tutorial for this tab on MAISRC's Youtube channel (opens in a new tab).",
                     style= "width: 80%; margin-bottom: 10px; display: block; margin-left: auto; margin-right: auto;"),
              tags$fieldset( #SEMANTICALLY APPROPRIATE DIV
              id = "records_formleft",
              tags$legend(class = "overview_filter_legends",
                          HTML("<span><b>Select Survey Records to View.</b> Use these options to select a specific lake and survey(s) to view.</span>")
                          ),

                  #CHECKBOX FOR FILTERING TO ONLY SPATIAL DATA.
       checkboxInput("only_spatial", 
                     label = HTML("<span class = 'boldthis specialleftpadding'>Feature: Exclude lakes lacking point-level location data.</span>"),
                     value = FALSE),
                    
       #SELECTOR FOR PICKING A LAKE
       selectizeInput("records_dows", 
                      label = HTML(
                                   "<span><b>Select a lake.</b> Delete all contents to use as a search bar. This is a required question; until you select a lake, the survey date picker below this question will not appear.</span>"),
                   choices = NULL,
                   options = list(render = I("{
      option: function(item, escape) {
        return '<div class=\"option\" role=\"option\" data-selectable data-value=\"' + escape(item.value) + '\">' + escape(item.label) + '</div>';
      }
    }"))),
       uiOutput("lakefinder_note"),
       #SELECTOR FOR PICKING THAT LAKE'S RECORDS
       hidden(shinyWidgets::pickerInput("records_surveys", 
                      HTML("<span><b>Select one (or more) survey(s).</b> Use control+click or click+drag to select multiples. Choose 'No selection' to pick a different lake. This is a required question; until you select at least one survey date, the tabs on the right-hand side of the screen will not populate.</span>"),
                      choices = NULL,
                      multiple = TRUE,
                      options = list(`actions-box` = TRUE, #GIVES SELECT AND DESELECT ALL BUTTONS,
                                     `selected-text-format` = "count > 2", 
                                     `count-selected-text` = "{0} survey dates")
                      ))
            
         )
        )
       ),
       #RIGHT-HAND MAIN PANEL
       column(width = 9,
              
              tags$main( #SEMANTICALLY APPROPRIATE DIV
              id = "records_formright",
              style = "max-width: 100%;",
              tags$h3("Survey records results display", class = "sr-only"), #SCREEN READER HEADING.
        
              
         tabsetPanel(id = "records_outputs",
                     selected = "1", 
                     type = "tabs",
                     tabPanel(title = "Littoral Freq. of Occurrence (LFOO)", 
                              value = "1",
                              div(class = "standardTable", 
                                  tags$h4("Littoral frequency of occurrence results tab", class = "sr-only"), #SCREEN READER HEADING.
                                  id = "surveys_metadata_div",
                                  style = "padding: 10px;",
                                  uiOutput("survey_metadata")
                              )
                     ),
                     tabPanel(title = "Abundance map", 
                              value = "2",
                              div(class = "standardTable", 
                                  id = "surveys_abundance_div",
                                  style = "padding: 10px;",
                                  tags$h4("Abundance map results tab", class = "sr-only"), #SCREEN READER HEADING.
                                  fluidRow(
                                    id = "abundance_tab_all",
                                  column(width = 4, 
                                         id = "records_form_abund", 
                                         hidden(tags$fieldset(
                                           id = "records_abund_fieldset",
                                           tags$legend(class = "overview_filter_legends",
                                                       HTML("<span><b>Survey-Specific Taxon Distribution.</b> Use these options to select a survey and taxon to view its distribution on the map or to download the map.</span>")),
                                         uiOutput("abundance_selectors")))),
                                  column(width = 8,
                                         id = "abundance_map_cell",
                                         uiOutput("abundance_data"))
                                  )
                              )
                     ),
                     tabPanel(title = "Raw survey data", 
                              value = "3",
                              div(class = "standardTable", 
                                  id = "theTableIsHere",
                                  style = "max-width: 100%;",
                                  #THIS DIV IS JUST HERE TO READ TO SCREEN READERS THE SEARCH RESULTS.
                                  tags$div(
                                    `aria-live` = "polite",
                                    `aria-atomic` = "true",
                                    class = 'sr-only',
                                    role = "status",
                                    id = "results_live_region",
                                    uiOutput("search_record_feedback")
                                  ),
                                  tags$h4("Raw survey data tab", class = "sr-only"), #SCREEN READER HEADING.
                                  uiOutput("rawTableCaptionDiv"),
                                  dataTableOutput("survey_table")     
                              )
                     )

       )
       )
      ),
   #DISCLAIMERS SECTION
   fluidRow(class = "flexthis width100 justifycenter flexcol", style = "margin-top: 10px;",
            tags$h3("Disclaimers section", class = "sr-only"), #SCREEN READER HEADING.
            actionButton("records_disclaimers",
                         label = '  Show disclaimers',
                         icon = icon("plus"),
                         class = "mapInputs",
                         `aria-expanded` = "false",
                         `aria-controls` = "records_disclaimers_div"),
            hidden(fluidRow(class = "mistakeNote", 
                            id = "records_disclaimers_div",
                            style = "margin-left: auto; margin-right: auto;",
                            HTML("<ul class = 'disclaimers'><li>If any species protected by <a href='https://www.dnr.state.mn.us/rsg/laws.html' target='_blank'>Minnesota State or Federal statutes</a> were found in any of the surveys made available on this tab, data from those taxa have been redacted in strict accordance with the law. As such, the data made available here may underrepresent the diversity and abundance of aquatic plants in the waterbodies for which we have records. However, the removed data are available upon request; contact MAISRC for more information.</li>
              <li>To respect the data sovereignty of <a href = 'https://gisdata.mn.gov/dataset/bdry-tribal-government' target = '_blank'>Tribal Nations located within Minnesota's boundaries</a>, all records provided to MAISRC for lakes within or intersecting Tribal lands are hidden from detailed public viewing on <b>P.I. Charter</b>. However, these data are available upon request with the permission from the respective Tribal Nation. If any Tribal Nations wish to make data from their lands publicly visible on <b>P.I. Charter</b>, please contact MAISRC.</li>
              <li>The Minnesota DNR has requested that we anonymize all surveyors who participated in a P.I. survey conducted by any of their programs. However, those data are available on request with permission from the respective program. Please contact the supervisor of the appropriate MN DNR program office for more information.</li>
              <li>All data displayed on <b>P.I. Charter</b> are accurate to the best of our current knowledge and capabilities. However, recognize that errors, inconsistencies, and inaccuracies may exist in the data presented; all data are made available 'as they are.' <a href='mailto:maisrc@umn.edu' target = '_blank'>Please contact MAISRC to report any potential issues with the data on <b>P.I. Charter</b></a>.</li>
              <li>Submissions must go through a QA/QC process before they are officially added to <b>P.I. Charter's</b> database. As such, the records made available on this tab may not reflect all records we have received to date. Please expect a delay in new submissions being added to this tab; if it has been longer than two weeks since a submission and a record is still not accessible here, however, contact MAISRC.</li></ul>")))
    )
  ),
  
  #THIS SCRIPT ENSURES THAT ARIA-LABELS GET PUT ON THE TWO TABSETS INDICATING TO KEYBOARD NAVIGATORS HOW THEY ARE SUPPOSED TO TRAVERSE THE TABS (ARROWS, NOT TAB.)

  tags$script(HTML("
  $(document).ready(function() {
    var checkExist = setInterval(function() {
      var $recordsTabs = $('#records_outputs');

      if ($recordsTabs.length > 0) {
        $recordsTabs.attr('aria-label', 'To navigate between tabs containing different types of visualizations, use the left and right arrow keys, and press Enter to activate a tab.');
        clearInterval(checkExist);
      }
    }, 100);
  });
  
  $(document).ready(function() {
    var checkExist = setInterval(function() {
      var $recordsTabs = $('#LFOO_tabset');

      if ($recordsTabs.length > 0) {
        $recordsTabs.attr('aria-label', 'To navigate between tabs containing different types of visualizations, use the left and right arrow keys, and press Enter to activate a tab.');
        clearInterval(checkExist);
      }
    }, 100);
  });
"))  
  
 )
}