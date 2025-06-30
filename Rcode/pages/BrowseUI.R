browseUI = function() {
  shiny::tabPanel(value = 1, 

    shiny::div(shiny::span(shiny::HTML('<span class="fa-solid fa-map" aria-hidden = "true"></span>'),"Overview")),
    
    ## Browse tab, top row (explainers) ----------------------------------------
    
    shiny::fluidPage( #Tab is all one fluid container.
      style = "padding-left: 0px; 
               padding-right: 0px;",
      tags$main( #SEMANTICALLY APPROPRIATE DIV
      
      #REMOVE THE LABEL ELEMENT FOR THE TAXON FILTERING RADIO BUTTONS GROUP AS IT IS EMPTY AND SCREEN READERS WON'T LIKE THAT.
      
      #THEN, ENSURE THAT ALL FILTERS ON THIS TAB HAVE ARIA LABEL ATTRIBUTES, AT LEAST, SO SCREEN READERS KNOW THEY ARE LABELED. THIS IS NECESSARY BECAUSE SEARCHABLE SELECTINPUTS REALLY HAVE TWO INPUTS (THE SEARCH BAR AND THE MENU) BUT JUST ONE LABEL. 
      tags$head(
        tags$script(HTML("
      $(document).ready(function() {
      var radioLabel = document.getElementById('taxon_filter_rule-label');
      if (radioLabel) {
        radioLabel.remove();
        document.getElementById('taxon_filter_rule').removeAttribute('aria-labelledby');
       }
      });
      
      $(document).ready(function() {
    var checkExist = setInterval(function() {
    var selectEl = document.getElementById('filter_taxa');
    if (selectEl) {
      selectEl.setAttribute('aria-label', 'Select taxa here.');
      // Now find the dropdown content element created by selectize
      var dropdown = document.querySelector('.selectize-dropdown-content[role=\"listbox\"]');
      if (dropdown) {
        dropdown.setAttribute('aria-label', 'List of available options');
      }
      clearInterval(checkExist); // Stop checking once the element is found
    }
      }, 100); // Check every 100 milliseconds
  });
  
  $(document).ready(function() {
    var checkExist = setInterval(function() {
    var selectEl = document.getElementById('filter_county');
    if (selectEl) {
      selectEl.setAttribute('aria-label', 'Select a county here.');
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
    var selectEl = document.getElementById('filter_dow');
    if (selectEl) {
      selectEl.setAttribute('aria-label', 'Select a lake here.');
      // Now find the dropdown content element created by selectize
      var dropdown = document.querySelector('.selectize-dropdown-content[role=\"listbox\"]');
      if (dropdown) {
        dropdown.setAttribute('aria-label', 'List of available options');
      }
      clearInterval(checkExist); 
    }}, 100);
  });

    "))
      ),
      
      #THIS JS CODE WATCHES FOR WHEN A SHINY-MODAL APPEARS AND THEN SETS IT AS A DIALOG BOX AND ARIA-MODAL, THEN ENSURES THAT FOCUS CAN SWITCH TO IT.
      tags$script(HTML("
  $(document).on('shown.bs.modal', '#shiny-modal', function () {
    var modal = document.getElementById('shiny-modal');

    // Set accessibility attributes
    modal.setAttribute('role', 'dialog');
    modal.setAttribute('aria-modal', 'true');
    modal.removeAttribute('tabindex');

    // Find focusable content inside modal and move focus
    var focusable = modal.querySelector('[tabindex], button, [href], input, select, textarea, [role=button]');
    if (focusable) {
      focusable.focus();
    }
  });
")),
     
      #THIS SCRIPT LOOKS FOR A LEAFLET MAP OF THE GIVEN ID AND MAKES ALL THE MARKERS TABBABLE. IT ALSO GIVES THEM A BUTTON ROLE AND AN ARIA-LABEL INDICATING IT'S A MAP MARKER AND THAT IT CAN BE ACTIVATED AND HOW. IT WORKS TO MAKE THEM TABBABLE AND ACTIVATABLE, BUT IT JUST MAKES THEM A KEYBOARD TRAP SINCE THERE ARE SO MANY OF THEM, AND THEY CAN'T EASILY BE UNIQUELY IDENTIFIED. SO, I ADVISE SCREEN-READER USERS TO EMPLOY A DIFFERENT METHODOLOGY. 
#       tags$script(HTML("
#   Shiny.addCustomMessageHandler('makeMarkersFocusable', function(message) {
#     var mapId = message.mapId;
# 
#     var tries = 0;
#     var maxTries = 20;
# 
#     var interval = setInterval(function() {
#       var mapContainer = document.getElementById(mapId);
#       if (!mapContainer) return;
# 
#       var markers = mapContainer.querySelectorAll('path.leaflet-interactive');
#       var updated = 0;
# 
#       markers.forEach(function(el) {
#         var d = el.getAttribute('d');
#         var isCircle = d && d.match(/a5,5/i); // circle marker, radius 5
# 
#         if (isCircle && !el.hasAttribute('tabindex')) {
#           el.setAttribute('tabindex', '0');
#           el.setAttribute('role', 'button');
#           el.setAttribute('aria-label', 'Lake marker'); // update this with a smarter label if desired
# 
#           el.addEventListener('keydown', function(e) {
#             if (e.key === 'Enter' || e.key === ' ') {
#               e.preventDefault();
#               el.dispatchEvent(new MouseEvent('click', {
#                 view: window,
#                 bubbles: true,
#                 cancelable: true
#               }));
#             }
#           });
# 
#           updated++;
#         }
#       });
# 
#       tries++;
#       if (updated > 0 || tries > maxTries) {
#         clearInterval(interval);
#       }
#     }, 300);
#   });
# ")),

#INSTEAD, THIS SCRIPT ENSURES THAT EVERY TIME THE LEAFLET MAP GETS REBORN ON THIS PAGE, IT GETS ARIA-HIDDEN AND LOSES KEYBOARD NAV.
tags$script(HTML("
  function scrubLeafletAccessibility() {
    var tries = 0;
    var maxTries = 20;

    var interval = setInterval(function() {
      var mapContainer = document.getElementById('interactivemap');
      if (!mapContainer) return;

      // Confirm map is rendered enough to modify
      var leafletSVG = mapContainer.querySelector('svg.leaflet-zoom-animated');

      if (leafletSVG || tries > maxTries) {
        // Remove ARIA and tabindex from container
        mapContainer.setAttribute('aria-hidden', 'true');
        mapContainer.setAttribute('tabindex', '-1');
        mapContainer.removeAttribute('aria-live');

        // Remove all tabbable children (zoom buttons, links, etc.)
        var focusables = mapContainer.querySelectorAll('a, button, [tabindex]');
        focusables.forEach(function(el) {
          el.setAttribute('tabindex', '-1');
          el.setAttribute('aria-hidden', 'true');
        });

        clearInterval(interval);
      }

      tries++;
    }, 300);
  }

  // Run once Shiny has booted
  document.addEventListener('DOMContentLoaded', function() {
    // Also re-run if map is re-rendered (optional hook if dynamic)
    Shiny.addCustomMessageHandler('scrubLeafletMap', function(msg) {
      scrubLeafletAccessibility();
    });

    // Run it now (initial map load)
    scrubLeafletAccessibility();
  });
")),


      tags$section( #SEMANTICALLY APPROPRIATE DIV
        class = "flexthis",
        tags$h3("Overview tab interactive menus", class = "sr-only"), #SCREEN READER HEADING.
        id = "info_filters_row",
        actionButton(inputId = "browse_info", 
                     label = "New to P.I. Charter? Start here!", 
                     class = "mapInputs"),

  ##Browse tab, top row (selectors) ----------------------------------------
  
  #It had been suggested that I put the interactive elements at the top of the page rather than at the bottom, so I'm moving the buttons and selectors up here.
  
  ### Browse tab, middle row, second column (selectors) ----------------------------------------
  #The second column contains selectors that serve to filter the markers shown on the screen at that moment. One can filter by lake, county, or taxon observed. None of the selectors allow multiple choices at present, but there is "cross-talk," such as selecting a county filtering the DOW selector down to just lakes in that county.
    actionButton("toggle_filters",
                 label = '  Click here for toggles/filters!',
                 icon = icon("plus"),
                 class = "mapInputs",
                 `aria-expanded` = "false",
                 `aria-controls` = "hidden_filters")),
  hidden(tags$section(id = "hidden_filters",
                      tags$h4("Map filters and toggles menu", class = "sr-only"), #SCREEN READER HEADING.
          tags$fieldset(
            id = "browse_filters_row",
            tags$legend(class = "overview_filter_legends",
              div(HTML("<span><b>Map Marker Filters.</b> Use these menus to filter the map\'s visible markers. Note: The county and lake filters have search bars; delete all content from the box, then type.</span>"),
                            htmlOutput("active_filters_conflict"))),
            class = "flexthis flexwrap width100 autoleftrightmarg justifycenter flexend",
                  shinyWidgets::pickerInput("filter_taxa", 
                                     label = div(
                                       shiny::HTML("Filter by taxon"),
                                                #ADD INTO THE LABEL HERE A RADIO BUTTONS WIDGET TO FIGURE OUT WHETHER THE MULTI-TAXON MATCH SHOULD MATCH ANY OR MATCH ALL.
                                                div(
                                                  radioButtons("taxon_filter_rule", 
                                                             label = NULL, 
                                                             choices = c("Match ANY", "Match ALL"),
                                                             selected = "Match ANY", 
                                                             inline = TRUE,
                                                             ))), 
                                     choices = db_allSearchableTaxa,
                                     multiple = TRUE, #MAKE A MULTI-SELECTOR  
                                     options = list(`actions-box` = TRUE, #GIVES SELECT AND DESELECT ALL BUTTONS
                                                    `selected-text-format` = "count > 1", #WHEN THE # SELECTED IS > 3, IT STOPS LISTING THEM ALL AND INSTEAD DISPLAYS X COLLECTED AS PER NEXT FORMATTING RULE. 
                                                    `count-selected-text` = "{0} taxa selected",
                                                    `none-selected-text` = "All taxa (default)")
                                     
                                    ),
                  selectInput("filter_county", 
                                     label = "Filter by county",
                                     choices = c("All",db_allCounties)),
                                              #This third selector has so many records available that we have to make it a server-side selectize, so we build it initially here with no choices and then update it server side. 
                  shiny::selectizeInput("filter_dow", 
                                        label = HTML("Filter to a specific lake  <a target= '_blank', href='https://maps1.dnr.state.mn.us/lakefinder/mobile/'>(Use LakeFinder for help!)</a>"),
                                        choices = "All")),

  ##Browse tab, top row (buttons) ----------------------------------------

  
  ### Browse tab, top row buttons, first column (clear/select all) ----------------------------------------
  #Above the map, there are two buttons, one that selects all the markers currently shown, and the other clears all markers selected up to that point and resets the map, selectors, and table accordingly.
  fluidRow(class = "flexthis flexwrap justifycenter",
           style = "margin-bottom: 10px;",
           id = "browse_all_buttons",
           tags$fieldset(id = "browse_toggles_first_three",
                         tags$legend(class = "overview_filter_legends",
                                     HTML("<span><b>Marker and Filter Options.</b>  Use these buttons to select all visible map markers, reset all selections, or toggle between scientific and common names.</span>")),
               shiny::actionButton("selectAll", "Select all markers", 
                               class = "mapInputs browse_buttons"),
           shiny::actionButton("clearActives", "Clear selections",
                               class = "mapInputs browse_buttons"),
  ### Browse tab, top row buttons, second column (name toggle) ----------------------------------------
  #Above the middle column, we insert a button that toggles between scientific and common names.
           shiny::actionButton("commonVsSci", "Toggle names",
                               class = "mapInputs browse_buttons")),
  
  ### Browse tab, top row buttons, third column (summary modal buttons) ----------------------------------------
  #Above the table are two buttons that, when pushed, generate modals that are custom-formatted and feature copy-able text. One reports all the lakes currently shown in the table; the other compiles a list of all unique taxa observed across all lakes currently shown in the table. A third button allows downloading the current table as a CSV file. 
       tags$fieldset(id = "browse_toggles_last_three",
                     tags$legend(class = "overview_filter_legends",
                            HTML("<span><b>Table Outputs.</b> Use these buttons to produce lists of lakes or unique taxa in the table.</span>")),
      shiny::actionButton("lakes_list", "Lake list",
                               class = "mapInputs browse_buttons"),

             shiny::actionButton("taxa_list", "Taxa list", 
                               class = "mapInputs browse_buttons")
           )
    )
   )),
 
 ## Browse tab, middle row (map, selectors, and table) ----------------------------------------
 
 tags$section( #Begin Row 2--This one houses the map and a table showing the data from each summary record for map markers that have been clicked. We partition each of these elements into its own column within this row.
   style = "max-width: 100%;",
   id = "mainOverviewContent",
   tabindex = "-1", 
   tags$h3("Lake records map and table", class = "sr-only"), #SCREEN READER HEADING.
   
   ### Browse tab, middle row, left column (map) ----------------------------------------
   #The first column holds an interactive map of the state, with circle markers wherever we have lakes with at least one record (mapped to the lake centroid).
   shiny::column(width = 4, 
                 id = "browse_map_cell",
                 style = "min-width: 320px; 
                          margin-bottom: 10px;",
                 tags$h4("Lake records map", class = "sr-only"), #SCREEN READER HEADING.
          tags$figure(
            tags$div(`aria-live` = "polite",
                     `aria-atomic` = "true",
                     role = "note",
                     style = "color: #4d4d4d; font-size: 80%;",
                     "Note for screen reader and keyboard users: This map is for mouse users only. To activate lakes for review, use the taxon, county, and lake filters in the filters and toggles menu above, then use the 'Select all' button to activate filtered lakes. Then, navigate to the table on this tab to explore those selected data."),
            
            leaflet::leafletOutput("interactivemap"),
            tags$figcaption("Map markers indicate lakes with survey data. To view more information, hover over or click a marker (long press and touch on mobile, respectively).",
                            id = "overview_map_cap"))
   ),

   ### Browse tab, middle row, right column (records table) ----------------------------------------
   #Column 2 of the 2nd row is the interactive records table--as you click map markers, the records from those selected lakes will appear here for browsing, searching, sorting, etc.
   shiny::column(width = 8, 
                 id = "browse_table_cell", 
                 tags$h4("Lake records table", class = "sr-only"), #SCREEN READER HEADING.
                 shiny::fluidRow(shiny::div(class = "standardTable",
                       id = "heresTheTable",
                       #THIS DIV IS JUST HERE TO READ TO SCREEN READERS THE SEARCH RESULTS.
                       tags$div(
                         `aria-live` = "polite",
                         `aria-atomic` = "true",
                         class = 'sr-only',
                         role = "status",
                         id = "results_live_region",
                         uiOutput("search_results_feedback")
                       ),
                       uiOutput("browseTableCaption"),
                       DT::dataTableOutput("interactivemapdf", height = "375px")
          ))), 
   
 )
  ),
 #DISCLAIMERS SECTION
 fluidRow(class = "flexthis", style = "flex-direction: column; width: 100%;",
          tags$h3("Disclaimers section", class = "sr-only"), #SCREEN READER HEADING.
   actionButton("browse_disclaimers",
                label = '  Show disclaimers',
                icon = icon("plus"),
                class = "mapInputs",
                `aria-expanded` = "false",
                `aria-controls` = "browse_disclaimers_div"),
   hidden(fluidRow(class = "mistakeNote", 
                   id = "browse_disclaimers_div", 
                   style = "margin-left: auto; margin-right: auto;",
       HTML("<ul class = 'disclaimers'><li>If any species protected by <a href='https://www.dnr.state.mn.us/rsg/laws.html' target='_blank'>Minnesota State or Federal statutes</a> were found in any of the surveys made available on this tab, the names of those taxa have been anonymized (e.g., \"Protected Species #1\") in strict accordance with the law. As such, the data made available here may underrepresent the diversity and abundance of aquatic plants in the waterbodies for which we have records. However, the unanonymized data are available upon request (use the contact link in the app\'s footer.).</li>
              <li>To respect the data sovereignty of <a href = 'https://gisdata.mn.gov/dataset/bdry-tribal-government' target = '_blank'>Tribal Nations located within Minnesota's boundaries</a>, all records provided to MAISRC for lakes within or intersecting Tribal lands are hidden from detailed public viewing on <b>P.I. Charter</b> (these markers are shown in green in the map on this tab). However, these data are available upon request with the permission from the respective Tribal Nation. If any Tribal Nations wish to make data from their lands publicly visible on <b>P.I. Charter</b>, please contact MAISRC (use the contact link in the app\'s footer.).</li>
              <li>The Minnesota DNR has requested that we anonymize all surveyors who participated in a P.I. survey conducted by any of their programs. However, those data are available on request with permission from the respective program. Please contact the supervisor of the appropriate MN DNR program office for more information.</li>
              <li>All data displayed on <b>P.I. Charter</b> are accurate to the best of our current knowledge and capabilities. However, recognize that errors, inconsistencies, and inaccuracies may exist in the data presented; all data are made available 'as they are.' Please contact MAISRC to report any potential issues with the data on <b>P.I. Charter</b> using the contact information provided in the app\'s footer.</li>
              <li>Submissions must go through a QA/QC process before they are officially added to <b>P.I. Charter's</b> database. As such, the records made available on this tab may not reflect all records we have received to date. Please expect a delay in new submissions being added to this tab; if it has been longer than two weeks since a submission and a record is still not accessible here, however, contact MAISRC using the contact link in the app\'s footer for an update.</li></ul>")))
   )
  )
 )
}