recordsServer = function(input, output, session) {
  
# One-time operations -----------------------------------------------------
  #SET UP A REACTIVES TRACKER FOR THIS TAB
  records_reactives = reactiveValues(
    avail_data = NULL, #THE MOST RECENTLY ACCESSED SURVEY DATA FILE
    processed_dat = NULL #DATA THAT HAS BEEN WORKED OVER A BIT MORE FOR THE SUB-TABS HERE.
  )
  records_waiter <- Waiter$new(id = "records_formright", 
                           html = spin_hexdots(),
                           color = "#ffb71e", )
  
  #Set up a debounce on Records_surveys so that users could realistically select multiple surveys before the app starts doing things.
  surveys_debounced = debounce(reactive({input$records_surveys}), 1250)
  
  #POPULATE THE DOW SELECTOR SERVER-SIDE, NO LONGER ANY NEED TO ADJUST THE LABEL.
  updateSelectizeInput(session, "records_dows", 
                       server = T,
                       choices = c("No selection", dowslist2use),
                       options = list(maxOptions = length(dowslist2use),
                                      render = I("{
      option: function(item, escape) {
        return '<div class=\"option\" role=\"option\" data-selectable data-value=\"' + escape(item.value) + '\">' + escape(item.label) + '</div>';
      }
    }")))
  
  #RENDER THIS AS A NOTE FOR USERS
  output$lakefinder_note = renderUI({
    div(HTML("If you're unsure of which lake you're interested in, <a target= '_blank', href='https://maps1.dnr.state.mn.us/lakefinder/mobile/'>use LakeFinder to locate it.</a>"),
        id="lakefinder_note")
  })
  
  

# Decluttering functions --------------------------------------------------
  #WHEN MAKING THE SUBTAB CONTENTS FOR THIS TAB, A LOT OF OPERATIONS NEED TO HAPPEN, SO IT'S EASIER TO JUST BUNDLE THEM INTO FUNCTIONS SO THAT THEY WILL BE EASIER TO FIND AND FIX AS WE GO.
  
  #FUNCTION FOR DETERMINING IF ALL THE CONTENTS OF A COLUMN ARE SOME MIXTURE OF NAS, BLANKS, FALSES, 0S, ETC.
  is_mixed_excludable <- function(column) {
    all(column %in% c(0, FALSE, "FALSE", "0", NA), na.rm=T)
  }
  
  #SAME AS ABOVE EXCEPT THAT TEXT VALUES COUNT AS "BLANKS" FOR DETERMINING IF A COLUMN IS "EMPTY" AND CAN THUS BE DITCHED. 
  is_mixed_excludable_text <- function(column) {
    all(column %in% c(0, FALSE, "FALSE", "0", NA) |
          is.na(suppressWarnings(as.numeric(column))), na.rm=T)
  }

  
  #TAKES THE INITIAL SURVEY RECORDS RAW DATA AND CUTS IT INTO A BIT MORE SHAPE BEFORE DISPLAYING ANY OF IT.
records_tabs_preprocessing = function(df) {
  
  df[df == ""] = NA #REPLACE ALL BLANKS WITH NAS
  
  #REPLACE BAD FALSES AND TRUES WITH REAL TRUES/FALSES
  df <- lapply(df, function(x) {
    if(is.character(x)) {
      x <- ifelse(x == "TRUE", TRUE, ifelse(x == "FALSE", FALSE, x))
    } else {
      x
    }
  })
  
  df <- as.data.frame(df) # CONVERT LIST BACK TO DF
  
  #HAVE TO REMOVE THE .1 AND .2 SUBSTRINGS AS NEEDED.
  adjusted_names = names(df)
  adjusted_names = gsub(".1", "", adjusted_names)
  adjusted_names = gsub(".2", "", adjusted_names)
  
  is.taxonomic = which(adjusted_names %in% tidyName(taxonomic)) #WHICH COLS TAXONOMIC?
  df[, is.taxonomic] = convert_column_types(df[, is.taxonomic]) #DO SMART TYPE CONVERSION ON ALL TAXONOMIC COLS TO SIMPLIFY THEM TO NUMERIC IF POSSIBLE

  #REDACT ANY PROTECTED SPECIES FROM THE TABLE AND ALSO THE OUTPUTTED FILE.
  df = df %>% 
    dplyr::select_if(.predicate = !adjusted_names %in% protected_species$taxon_tidy) %>% 
    dplyr::select(-SUBMITTER_EMAIL) %>% #ALSO, REMOVE SUBMITTER_EMAIL COLUMN
    dplyr::select_if(~!is_mixed_excludable(.)) #REMOVE ANY COLUMNS THAT ARE ENTIRELY NA OR 0 or FALSE FOR NEATER REPORTING BUT MAINTAIN ANY TEXT VALUES FOR NOW.
  
  #IF ANY MN DNR DATA ARE HERE, REDACT THE SURVEYORS' NAMES.
  df$SURVEYORS[grepl("DNR", df$SUBMITTER_NAME)] = "[Redacted--contact the supervisor of the MN DNR program listed in the 'SUBMITTER_NAME' field for more information]"
  
  records_reactives$avail_data <- df #CACHE THIS VERSION OF THE DATA SET--THIS ONE CAN GO ON TO BE USED IN FUTURE FUNCTIONS AND ALSO BE AVAILABLE FOR THE DOWNLOAD HANDLER.
  return(df)
}
  
#GENERATE THE DATA THAT WILL POPULATE THE FREQ OF OCCURRENCE SUBTAB.
 records_LFOO_subtab = function(df) {

   names2match = tidyName(gsub(".1", "", x = names(df))) %>%  #REMOVE FUNNY .1S AND TIDY.
     gsub(".2", "", x = .)

   data_graph = df %>%  #FIND ONLY TAXONOMIC COLS AND DEPTH STUFF IN CURRENT SURVEY RECORDS DATA.
     dplyr::select_if(.predicate = names2match %in%
                        c(tidyName(taxonomic), "survey_start", "depth_ft"))
   
   #CONVERT ALL BUT DATE AND DEPTH UNITS TO NUMERIC
   data_graph = suppressWarnings(data_graph %>% 
                                   mutate_if(.predicate = !names(data_graph) %in%
                                               c("SURVEY_START"), as.numeric))
   
   data_graph[is.na(data_graph)] = 0 #REPLACE ALL NAS WITH 0S
   
   #YANK OUT WHOLE RAKE FOR THIS PURPOSE IF IT EXISTS
   if("whole_rake_density" %in% names(data_graph)) {
     data_graph = data_graph %>% 
       select(-whole_rake_density)
   }
   
   #FOR EACH SURVEY, CALCULATE POPULATION FREQUENCY OF OCCURRENCE (% COVER) FOR EACH TAXON AND FOR ALL PLANTS AS A WHOLE
   data_graph2 <- data_graph[, sapply(data_graph, function(x) !all(x == 0))] #EXCLUDE ALL COLS CONTAINING ONLY 0S
  
   #REDUCE ALL TAXONOMIC DATA TO PRESENCE/ABSENCE AT THIS POINT
   notThese = which(names(data_graph2) %in% c("SURVEY_START", "depth_ft")) 
   data_graph2[-notThese] = 
     apply(data_graph2[-notThese], c(1,2), function(x) { ifelse(x > 0, 1, 0)})
 
   data_graph3 = data_graph2 %>% 
     select(-contains(c("drepanocladus", "fontinalis", "sponge", "algae", "wolffia", "lemna",
                        "spirodela", "azolla", "ricciocarpos", "riccia", "pistia", "eichhornia",
                        "cyanobacteria", "nostoc", "sphagnum", "spirogyra", "unk_sp"
     ), ignore.case = T)) %>%  #REMOVE, AS PER WI PROTOCOL, ALL FLOATING AND NON-VASCULAR PLANTS. THANKS TO MEGAN WEBER AND MICA KROMREY FOR HELPING ME BUILD THIS LIST. FOR NOW, USING THE GENERA NAMES IS OK, BUT MIGHT BE PROBLEMATIC IF MORE MEMBERS OF THESE GENERA ARE FOUND THAT SHOULDN'T COUNT (UNLIKELY). 
     mutate(total = rowSums(select(., where(is.numeric), -any_of("depth_ft")))) %>% #COUNT OF HOW MANY TAXA WERE OBSERVED AT EACH SAMPLING PT. ANY_OF HELPS PROTECT US IF A SURVEY LACKS ANY DEPTH DATA.
     mutate(total01 = ifelse(total > 0, 1, 0)) %>%  #REDUCE THAT TO PRESENCE/ABSENCE OF ANY PLANTS AT A POINT
     group_by(SURVEY_START) %>% #GROUP BY SURVEY
     mutate(`Points sampled (N)` = n(), #DETERMINE NUMBER OF PTS SURVEYED.
            `Points occupied (N*)` = sum(total01),  #DETERMINE NUMBER OF SURVEYED PTS OCCUPIED BY ANYONE. 
            `Max occupied depth (ft)` = if("depth_ft" %in% names(.)) max(depth_ft, na.rm = TRUE) else "No data") %>% #CALC MAX OCCUPIED DEPTH ONLY IF A DEPTH_FT COLUMN EXISTS
     ungroup() %>% #UNGROUP
     filter(total01 > 0) %>% #EXCLUDE ALL SAMPLE POINTS NOT YIELDING ANY PLANTS (INHOSPITABLE)
     select_if(.predicate = !names(.) %in% c("total", "total01", "depth_ft")) %>% #REMOVE THOSE COLS, NOW THAT WE'VE FILTERED TO N*, IF THEY EXIST TO BE DE-SELECTED (WHICH DEPTH MAY NOT!)
     group_by(SURVEY_START) %>% #GROUP BY SURVEY AGAIN
     summarize(`Points sampled (N)` = first(`Points sampled (N)`), #KEEP THESE TWO COLS AS IS
               `Points occupied (N*)` = first(`Points occupied (N*)`),
               `Max occupied depth (ft)` = first(`Max occupied depth (ft)`), 
               across(-c(`Points sampled (N)`, 
                         `Points occupied (N*)`,
                         `Max occupied depth (ft)`),
                      ~round((sum(.) / n()) * 100, 1)))  %>%  #CALC PRECENT COVER FOR EACH TAXON (ALL OTHER COLS) FOR EACH SURVEY
     arrange(desc(as.Date(SURVEY_START))) #ARRANGE BY DATES IN ASCENDING ORDER (MOST RECENT FIRST) 
   
   #REORDER THE COLUMNS IN LEFT TO RIGHT ORDER BY DECREASING AVG PERCENT COVER.
   col_means = colMeans(data_graph3[5:ncol(data_graph3)]) #GET THE COL AVGS OF ALL BUT THE LEADER COLS
   new_order = names(data_graph3)[5:ncol(data_graph3)][order(col_means, decreasing = T)] #USE ORDER FUNCT TO RE-ORDER THE NAMES BY THOSE COL MEANS
   data_graph4 = data_graph3[c(names(data_graph3)[1:4], new_order)] #IMPOSE THAT NEW ORDER
   
   #PASTE ON A % SIGN TO THE PERCENT COVER DATA (THAT'S ALL THIS BLOCK DOES)
   data_graph5 = data_graph4 %>% 
     mutate(across(-c(SURVEY_START,
                      `Points sampled (N)`,
                      `Points occupied (N*)`,
                      `Max occupied depth (ft)`),
                   function(x) {
                     paste0(x, "%") 
                   }))
   
   #ADD SOME BOLDING TO THE FRONT MATTER OF THIS SUBTAB AND THEN INSERT LISTS OF SURVEYS AND THE NUMBER THEREOF
   survey_ui_list <- HTML("<p><span class = boldthis>Survey date(s) selected: </span>",
                          paste0(data_graph5$SURVEY_START, collapse = ", "),
                          "</p>")
   
   output$taxon_pct_covers_table = renderDT({
     
     dat_tmp1 = cbind(data_graph5[,1:4], #PUT SURVEY_START DATE AND N DATA FIRST
                      data.frame(`Plant LFOO` = 
                                   round((data_graph5$`Points occupied (N*)`/data_graph5$`Points sampled (N)`) * 100, 1) %>%
                                   paste0(.,  "%"), #THEN THE NEW COMMUNITY LFOQ
                                 data_graph5[,5:ncol(data_graph5)])) #THEN THE TAXON-SPECIFIC LFOQS 
     
     #REPLACE THE TAXONOMIC COLUMN NAMES TO BE MORE HUMAN READABLE 
     names(dat_tmp1)[6:ncol(dat_tmp1)] = replace_names(names(dat_tmp1)[6:ncol(dat_tmp1)],
                                                       commonsciNameLookup$newfieldname,
                                                       commonsciNameLookup$sciname)
     #INSERT ITALIC SPANS AROUND THEM SO THEY ARE FORMALLY CORRECT.
     names(dat_tmp1)[6:ncol(dat_tmp1)] = paste0("<span class = italicize>", 
                                                 names(dat_tmp1)[6:ncol(dat_tmp1)],
                                                 "</span>")
     
     names(dat_tmp1)[c(1,5)] = c("Survey start date", "Plant LFOO") #FIX THE FORMATTING OF THESE TWO COL NAMES.
     
     #GET RID OF ANY .1S OR .2S THAT HAVE GOTTEN INTRODUCED.
     names(dat_tmp1) = gsub(".1", "", names(dat_tmp1)) %>% 
       gsub(".2", "", .)
     
     dat_tmp1$`Points occupied (N*)` = as.integer(dat_tmp1$`Points occupied (N*)`) #FIX THE TYPING ON THIS COL SO IT DOESN'T DISPLAY ANY DECIMALS.
     
     #IF ALL THE DEPTH DATA WERE NAs, YOU'D END UP WITH -INFs HERE, WHICH WE WILL OVERWRITE WITH A MORE INFORMATIVE ENTRY.
     if(!any(is.na(as.numeric(dat_tmp1$`Max occupied depth (ft)`)))) { #DON'T PROCEED IF ANY ARE ALREADY 'NO DATA'
       if(any(is.infinite(dat_tmp1$`Max occupied depth (ft)`) | #ONLY PROCEED IF -INF OR 0
              round(dat_tmp1$`Max occupied depth (ft)`, 0) == 0)) {
         dat_tmp1$`Max occupied depth (ft)`[is.infinite(dat_tmp1$`Max occupied depth (ft)`) | round(dat_tmp1$`Max occupied depth (ft)`, 0) == 0] = "No data"
       }
     }
     
     datatable(dat_tmp1, 
               fillContainer = TRUE,
               escape = FALSE,
               rownames = FALSE, #TO PREVENT AN EMPTY HEADER ROW, WHICH IS BAD FOR ACCESSIBILITY. 
               caption = tags$caption("Littoral frequency of occurrence data by survey date (rows) and taxon (columns).",
                                      style = "max-width: 62vw; 
                                                  word-wrap: break-word;"),
               selection = 'none', #NO ROW SELECTION
               options = base::list(
                 searching = FALSE,
                 paging = FALSE,
                 info = FALSE,
                 scrollX = TRUE,
                 scrollY = FALSE,
                 processing = FALSE, #NO SPINNER
                 pageLength = nrow(dat_tmp1), #LOCK LENGTH TO EXACTLY THE NUMBER OF SURVEYS THAT WERE SELECTED.
                 lengthChange = FALSE),
               callback = DT_A11Y_Callback
               )  #PRINT THE FINAL TABLE
     
   })
   
   
   ##MAKING THE APPROPRIATE PLOTLY GRAPHS
   if(nrow(data_graph4) == 1) { #IF A SINGLE SURVEY WAS SELECTED
     
     #GRAB JUST THE TAXONOMIC DATA
     data_plotly = data_graph4[1, 5:ncol(data_graph4)]
     
     #ORDER THE DF'S TAXA LARGEST TO SMALLEST LFOO
     col_means = colMeans(data_plotly) #GET THE COL AVGS
     new_order = names(data_plotly)[order(col_means, decreasing = T)] #USE ORDER TO SORT THE NAMES ACCORDINGLY.
     data_plotly2 = data_plotly[new_order] #IMPOSE THAT NEW ORDER
     
     #REPLACE THE TAXONOMIC COLUMN NAMES TO BE MORE HUMAN READABLE 
     names(data_plotly2) = replace_names(names(data_plotly2),
                                         commonsciNameLookup$newfieldname,
                                         commonsciNameLookup$sciname)
     
     
     #GET RID OF ANY .1S OR .2S THAT HAVE GOTTEN INTRODUCED.
     names(data_plotly2) = gsub(".1", "", names(data_plotly2)) %>% 
       gsub(".2", "", .)
     
     #GET APPROPRIATELY ABBREVIATED NAMES BEFORE WE PUT IN ITALICS SPANS
     names2abbr = abbreviate_names(names(data_plotly2))
     
     #INSERT ITALIC SPANS AROUND THEM SO THEY ARE FORMALLY CORRECT. #****This first block may not be needed any longer...
     names(data_plotly2) = paste0("<i>", 
                                  names(data_plotly2),
                                  "</i>")
     
     names2abbr = paste0("<i>", 
                         names2abbr,
                         "</i>")

     #ORGANIZE INTO A DATA FRAME THE TAXONOMIC NAMES AND THEIR DATA.
     data_plotly3 = data.frame(val = as.numeric(data_plotly2[1,]),
                               taxon = names(data_plotly2))
     
     #CREATE A TOOLTIP TEXT COL PREPOPULATED WITH WHAT I WANT THE TOOLTIPS TO SAY
     data_plotly3 = data_plotly3 %>% 
       mutate(tooltip_text = paste0("Taxon: ", taxon,"<br>",
                                    "LFOO: ", round(as.numeric(val, 0)), "%"))
     
     #Approp. font sizes
     num_taxon = length(unique(data_plotly3$taxon))
     if(num_taxon >= 32) {
       font_size = 12
     } else { font_size = 14}

     
     #BUILD THE PLOTLY BAR GRAPH
     LFOO_plot = plot_ly(data = data_plotly3,
                         y = ~names2abbr,
                         x = ~round(as.numeric(val), 0),
                         hoverinfo = "text", #THIS FORCES ONLY MY CUSTOM TEXT (NEXT ARG) TO DISPLAY ON THE TOOLTIPS
                         text = ~tooltip_text,
                         textposition = "none", #DO NOT PUT MY TEXT ON THE BARS TOO
                         type = 'bar', #SHORTCUT TO CREATING A BAR GRAPH
                         marker = list(color = "#7a0019")) %>% #IMPOSE A SINGLE BAR COLOR.
       layout(paper_bgcolor = 'white', 
              plot_bgcolor = 'white',
              xaxis = list(title = "<b>Littoral freq. of occurrence (%)</b>",
                           gridcolor = 'lightgrey', 
                           linecolor = "black", 
                           linewidth = 2, 
                           tickfont = list(family = "Open Sans", color='black', size = 14),
                           titlefont = list(family = "Open Sans", color="black", size = 18),
                           fixedrange = TRUE), 
              yaxis = list(categoryarray = unique(names2abbr), 
                           title = "", 
                           linecolor = "black", 
                           linewidth = 2, 
                           tickfont = list(family = "Open Sans", color='black', size = font_size),
                           fixedrange = TRUE
              ))
     
   } else {
     #OTHERWISE, IF MULTIPLE SURVEYS WERE SELECTED
     
     #STILL GRAPH JUST THE TAXONOMIC COLUMNS BUT ALSO THE SURVEY_START COLUMN.
     data_plotly = data_graph4[,c(1,5:ncol(data_graph4))]
     
     #REPLACE THE TAXONOMIC COLUMN NAMES TO BE MORE HUMAN READABLE 
     names(data_plotly)[2:ncol(data_plotly)] = replace_names(names(data_plotly)[2:ncol(data_plotly)],
                                                             commonsciNameLookup$newfieldname,
                                                             commonsciNameLookup$sciname)
     
     #Gather into long format for easier plotting. The only use for tidyr at time of writing...
     data_plotly2 = tidyr::pivot_longer(data_plotly, 
                                        cols = -SURVEY_START, 
                                        names_to = "taxon", 
                                        values_to = "LFOO")
     
     endRange = ncol(data_plotly)
     
     #GET APPROPRIATELY ABBREVIATED NAMES BEFORE WE PUT IN ITALICS SPANS
     data_plotly2 = data_plotly2 %>% 
       mutate(taxon_abbr = abbreviate_names(taxon)
         ) %>% 
       mutate(taxon = paste0("<i>", taxon, "</i>"),
              taxon_abbr = paste0("<i>", taxon_abbr, "</i>"))

     #BUILD A DISCRETE VIRIDIS ROCKET COLOR PALETTE FOR OUR TAXA
     viridis_cols = viridis::rocket(n = length(unique(data_plotly2$taxon)))
     
     #CREATE CUSTOM TOOLTIP TEXT
     data_plotly2 = data_plotly2 %>% 
       mutate(tooltiptext = paste0("Taxon: ", taxon, "<br>",
                                   "LFOO: ", LFOO, "%<br>",
                                   "Survey date: ", SURVEY_START))
     
     #BUILD THE PLOTLY LINE GRAPH
     LFOO_plot = plot_ly(data = data_plotly2,
                         x = ~as.Date(SURVEY_START),
                         y = ~round(as.numeric(LFOO), 0),
                         color = ~taxon_abbr,
                         colors = viridis_cols,
                         type = 'scatter', #SHORTCUTS TO A LINE GRAPH
                         mode = 'line',
                         hoverinfo = "text",
                         text = ~tooltiptext) %>% 
       layout(paper_bgcolor = 'white', 
              plot_bgcolor = 'white',
              yaxis = list(title = "<b>LFOO (%)</b>",
                           gridcolor = 'lightgrey', 
                           linecolor = "black", 
                           linewidth = 2, 
                           tickfont = list(family = "Open Sans", color='black', size = 14),
                           titlefont = list(family = "Open Sans", color="black", size = 18),
                           fixedrange = TRUE), 
              xaxis = list(title = "", 
                           linecolor = "black", 
                           linewidth = 2, 
                           tickfont = list(family = "Open Sans", color='black', size = 14),
                           fixedrange = TRUE
              ),
              legend = list(
                orientation = 'h',   # Horizontal legend
                x = 0,               # Position at the left
                y = -0.3          # Position below the x-axis
              )
              )
     
     
   }
   
   #CREATE SOME SLIGHTLY DIFFERENT HEADER AND INSTRUCTION TEXT DEPENDING UPON WHICH GRAPH WE'RE MAKING.
   if(nrow(data_graph5) == 1) {
     caption_string = "Littoral frequency of occurrence (LFOO; x-axis) by taxon (y-axis). These data are available in a table on the Table subtab."
     instruction_string = "<p class = 'smaller-text'>This graph has some handy features:</p>
       <ul>
       <li>Hover over bars to view details (or long-press on mobile).</li>
       <li>In the menu bar, hit the camera icon to save a picture of the graph.</li>
       </ul>"
     shinyjs::hide("graph_features")
   } else {
     caption_string = "Littoral frequency of occurrence (LFOO; y-axis) by taxon (colors) and survey date (x-axis). These data are available in a table on the Table subtab."
     shinyjs::show("graph_features")
     instruction_string = "<i class = 'smaller-text'><p>This graph has several handy features:</p>
       <ul>
       <li>Hover over points to view details (long-press on mobile devices).</li>
       <li>Click a taxon in the legend to remove it from view. Click again to restore.</li>
       <li>In the menu bar, hit the camera icon to save a picture of the graph.</li>
       <li>Also in the menu bar, click 'Compare data on hover' to compare multiple taxa when hovering (or long-pressing on mobile).</li>
       </ul></i>"
   }
   
   ##ACTUALLY RENDER THE PLOTLY GRAPH WE'VE MADE, WHICHEVER IT WAS, AND OUTPUT IT.
   output$LFOO_graph_UI = renderUI({ 
     
     output$LFOO_graph = renderPlotly({ LFOO_plot })
     
     tags$figure(
       tags$figcaption(caption_string,
                       id = "records_plotly"),
       plotlyOutput("LFOO_graph", height = "100%")
     )
     
   })
   
   #DISPLAY IN UI A TAGLIST OF % COVER OUTPUTS PRODUCED ABOVE.
   output$survey_metadata <- renderUI({
     
     #THE REST OF THE LFOO SUBTAB UI CONTENT
     tagList(
       HTML("<p><span class = boldthis>Number of surveys selected: </span>", #NUM SURVEYS SELECTED
            paste0(nrow(data_graph5)), 
            "</p>"),
       survey_ui_list, #WHICH SURVEYS THOSE ARE

       #THE TWO SUB-PANELS ON THIS SUB-PANEL
       tabsetPanel(id = "LFOO_tabset", 
                   selected = "1",
                   type = "tabs",
                   footer = #INFO ON HOW THE DATA WERE GENERATED, NOW HIDDEN BEHIND A MENU BUTTON.
                     fluidRow(class = "flexthis", 
                              style = "flex-direction: column; margin-top: 10px; max-width: 100%; margin-left: 0px !important; margin-right: 0px !important;",
                              actionButton("LFOO_explanation",
                                           label = '  Define LFOO',
                                           icon = icon("plus"),
                                           class = "mapInputs"),
                              hidden(fluidRow(class = "mistakeNote", 
                                              id = "LFOO_explanation_div", 
                                              style = "margin-left: auto; margin-right: auto;",
                                              HTML("<br>
                    <div class='col-sm-12' align = 'center'>
                    <h3>What is littoral frequency of occurence (LFOO)?</h3></div>
                   <ul>
                  <li>Littoral frequency of occurence (LFOO) is the percent of sampling locations ('points') actually occupied by a specific taxon or any taxon, depending upon the desired scale.
                  <li>To calculate it, we first find the number of sampled points sampled in a survey (N). Then, we find the number of points were any taxa were observed (N*). For the latter, organisms from the following genera are excluded: <i>Drepanocladus</i>, <i>Fontinalis</i>, <i>Wolffia</i>, <i>Lemna</i>, <i>Spirodela</i>, <i>Azolla</i>, <i>Ricciocarpos</i>, <i>Riccia</i>, <i>Pistia</i>, <i>Sphagnum</i>, and <i>Eichhornia</i>. Freshwater sponges, cyanobacteria, <i>Nostoc</i> species, <i>Spirogyra</i> species, taxa marked as 'unknown', and algae were also excluded.</li>
                   <li>Plant LFOO is N*/N x 100 and represents the estimated percentage of a lake's hospitable, littoral area occupied by plants.</li>
                   <li>Taxon LFOO is calculated using the number of points occupied by a taxon (X) via X/N* x 100 and represents the estimated percentage of a lake's hospitable, littoral area occupied by that taxon, though likely an underestimate.</li>
                   <li>For all calculations, only plants caught on a surveyors' rake matters; any taxa observed only visually (when indicated) are excluded.</li>")                   
                  ))),
                   tabPanel(title = "Graph", 
                            value = "1",
                            fluidRow(class = "flexthis", style = "flex-direction: column; max-width: 100%; margin-left: 0px !important; margin-right: 0px !important; flex-wrap: wrap; align-items: center;",
                                     #SPECIAL GRAPH FEATURES, ALSO HIDDEN BEHIND A BUTTON
                                     actionButton("graph_features",
                                                  label = '  Show graph instructions',
                                                  icon = icon("plus"),
                                                  class = "mapInputs"),
                                     hidden(fluidRow(class = "mistakeNote", 
                                                     id = "graph_features_div", 
                                                     style = "margin-left: auto; margin-right: auto;",
                                HTML(instruction_string)                    
                                ))),
                            fluidRow(column(width = 12, align="center",
                                   uiOutput("LFOO_graph_UI")))), #THE GRAPH ITSELF
                   tabPanel(title = "Table",
                            value = "2",
                            fluidRow(column(width = 12, align="center",
                            DTOutput("taxon_pct_covers_table"))) #DT OF LFOO DATA, AS RENDERED ABOVE.
                            ))
     )
   })
  
 }
 
 #BUILDS THE SELECTORS MENU FOR THE ABUNDANCE SUB-TAB UPON SELECTION OF A SPECIFIC SURVEYS LIST OF INTEREST.
 records_rakes_subtab_selector1 = function(df) {
   
   #HERE ARE THE DATES TO POPULATE THE SURVEY SELECTOR WITH.
   dates2pick = as.character(sort(as.Date(unique(df$SURVEY_START)), decreasing = TRUE))
   
   shinyjs::show("records_abund_fieldset")
   
   output$abundance_selectors <- renderUI({
     
     tagList(
       selectInput("abundance_survey",
                   label = "Select a survey.", 
                   choices = dates2pick),
       selectInput("abundance_taxon", 
                   label = "Select a taxon observed during that survey.",
                   choices = NULL), #AT FIRST, THE TAXON SELECTOR WILL BE EMPTY, BUT WE WILL SOON POPULATE IT.
       downloadButton("abundance_map_download",
                      "Download map as HTML", 
                      class = "mapInputs")
     )
     
   })
   
 }
 
 #THIS FUNCTION GETS CALLED RIGHT AFTER INPUT$ABUNDANCE_SURVEY CHANGES OR GETS POPULATED SO THAT WE CAN POPULATE THE TAXON SELECTOR ON THE ABUNDANCE SUBTAB.
 records_rakes_subtab_selector2 = function(df) {

   df[is.na(df)] = 0 #REPLACE NAS THRUOUT THE DATA WITH 0S

   df = df %>% 
     filter(SURVEY_START == input$abundance_survey) #KEEP ONLY RECORDS FROM THE SURVEY THE USER HAS SELECTED (OR WHATEVER WE'VE DEFAULTED TO)
   
   df = df[, !sapply(df, is_mixed_excludable_text) | 
                names(df) %in%
             c('SURVEY_START', 'RAKE_MAX', 'longitude', "latitude")] #ELIMINATE ANY COLUMNS THAT SHOULD BE EXPENDABLE AT THIS POINT, INCLUDING THOSE CONTAINING ANY MIXTURE OF "NOTHING" PLUS TEXT ENTRIES WHICH WOULD BE HARD TO PLOT ON A MAP
   
   #START BUILDING THE FINAL DATA SET TO MAP
   names2match = tidyName(gsub(".1", "", x = names(df))) %>%  #REMOVE FUNNY .1S AND SUCH FROM NAMES AND TIDY.
     gsub(".2", "", x = .)

   data_map = df %>%  #FIND ONLY TAXONOMIC COLS, LOCATION STUFF, AND RAKE UNITS IN CURRENT SURVEY RECORDS DATA AND SELECT DOWN TO JUST THOSE.
     dplyr::select_if(.predicate = names2match %in%
                        c(tidyName(taxonomic), "survey_start", "latitude", "longitude", "rake_max"))
   
   names(data_map) = gsub(".1", "", x = names(data_map)) %>%  #REMOVE FUNNY .1S AND TIDY, THIS TIME FROM THE DATA SET ITSELF (NAMES2MATCH NEEDS TO BE A SEPARATE OBJECT, OTHERWISE .1S AND .2S IN THE DATASET WOULDN'T GET PROPERLY MATCHED)
     gsub(".2", "", x = .)
   
   taxa_available = names(data_map) #CREATE VECTOR FOR POPULATING THE SELECTOR.
   taxa_available = taxa_available[!taxa_available %in%
                                     c("SURVEY_START", "latitude", "longitude", "RAKE_MAX")] #DON'T INCLUDE THESE NON-TAXONOMIC COLUMNS
   
   #SUBSTITUTE IN LESS TIDIED, MORE PRETTY VERSIONS FOR USERS TO SEE IN THE SELECTOR.
   pretty_names = replace_names(taxa_available, 
                                commonsciNameLookup$newfieldname, 
                                commonsciNameLookup$sciname)
   
   #PRETTIFY WHOLE RAKE DENSITY, IF IT EXISTS.
   if("whole_rake_density" %in% pretty_names) {
     pretty_names[pretty_names == "whole_rake_density"] = "Whole rake density"
   }
   
   names(taxa_available) = pretty_names #ATTACH THE PRETTY AND TIDY VERSIONS
   
   taxa_available = sort(taxa_available) #SORT ALPHABETICALLY.
   
   #IF THE FIRST TAXON IN A SECOND SURVEY SELECTED JUST SO HAPPENS TO BE THE SAME TAXON THAT WAS ALREADY SELECTED IN THE FIRST SURVEY, THAT WILL NOT REGISTER AS AN INPUT CHANGE FOR INPUT$ABUNDANCE_TAXON. HERE, WE FORCE ONE TO OCCUR ANYWAY BY DOING A QUICK SWITCH BETWEEN TWO AVAILABLE CHOICES, IF POSSIBLE.
   prev_taxon = input$abundance_taxon #WHAT IS THE PREV TAXON?
   
   if(isTruthy(prev_taxon) && #IF THIS ISN'T NULL
      prev_taxon == taxa_available[1] && #AND THIS PREV TAXON MATCHES WHAT WILL BE THE FIRST TAXON IN THE NEW LIST.
      length(taxa_available) > 1) { #AND THERE IS MORE THAN ONE NEW CHOICE AVAILABLE...

     #SWAP TO THE SECOND TAXON AVAILABLE INSTEAD
     updateSelectInput(session = session,
                       "abundance_taxon",
                       choices = taxa_available,
                       selected = taxa_available[2])
   }

   #THEN, SWAP (BACK) TO THE NEW LIST AND FIRST ENTRY
   updateSelectInput(session = session,
                     "abundance_taxon",
                     choices = taxa_available,
                     selected = taxa_available[1])

   records_reactives$processed_dat <- data_map #STASH THIS AS A SLIGHTLY MODIFIED VERSION FOR THE MAP TO THEN USE.

 }
 
 
 records_rakes_subtab_map = function(df) { 

     data_map = df #LAZY RENAMING FOR CONVENIENCE

     data_map = data.frame(data_map) #REPAIR DUPE NAMES

     #CONVERT ALL BUT METADATA COLUMNS TO FULLY NUMERIC
     data_map = suppressWarnings(data_map %>%
                                     mutate(across(which(!names(.) %in% c("SURVEY_START", "latitude", "longitude", "RAKE_MAX")), as.numeric)))

     #FILTER TO ONLY THE SELECTED SURVEY AND TAXON
     data_map2 = data_map %>%
       dplyr::select(sym(input$abundance_taxon), latitude, longitude, RAKE_MAX) %>%
       rename(taxon = sym(input$abundance_taxon)) %>%
       arrange(taxon)

     data_map2$taxon = round(data_map2$taxon, 0) #ROUND OFF THE VALUES TO WHOLE NUMBERS, AS NEEDED.

     data_map3 = data_map2 %>% 
       filter(round(latitude, 0) != 0,
              round(longitude, 0) != 0) %>%  #NO SENSE IN PLOTTING THOSE ROWS WITHOUT ANY LOC DATA, AS THOSE WILL JUST END UP PLOTTING OVER BY AFRICA LOL. THESE WOULD BE 0S (FORMER NAS) BY THIS POINT
       st_as_sf(coords = c("longitude", "latitude"), crs=4326) #ENSURE DATA ARE CLASSED AS SPATIAL IN THE RIGHT CRS.

     #BEAUTIFY THE TAXON NAME
     taxon_name = replace_names(input$abundance_taxon, 
                                toreplace = commonsciNameLookup$newfieldname,
                                replacements = commonsciNameLookup$sciname)
     
     #REFORMAT WHOLE RAKE DENSITY'S NAME
     if(taxon_name == "whole_rake_density") {
       taxon_name = "All plants"
     }
     
     output$abundance_data <- renderUI({
       
       output$abundance_map <- renderLeaflet({
 
         #IF MAPPING WET WEIGHTS FROM JILL, WE NEED TO DO SOMETHING DIFFERENT...
         if(any(grepl("Wet", data_map3$RAKE_MAX), na.rm=T)) {
           
           #USE A NUMERIC PALETTE
           map_abund_pal = colorNumeric(palette = "viridis", 
                                        domain = NULL,
                                        na.color = "gray")
           
           #HAVE A DIFFERENT SUBSTRING IN THE LEGEND TITLE
           rake_lab = "<br>wet mass (g)"
           
           #AND LIMIT # BINS IN LEGEND.
           bins1 = TRUE
           
         } else {

           #IF WE KNOW THE MAX VALUE, WE CAN USE IT AS THE ENDPOINT FOR THE PALETTE'S DOMAIN. OTHERWISE, WE HAVE TO USE NULL TO DETERMINE IT LATER. 
           if(data_map3$RAKE_MAX[1] == "Unknown") {
              range_tmp = NULL
           } else {
              range_tmp = as.factor(0:as.numeric(data_map3$RAKE_MAX[1]))
           }
           
           #OTHERWISE, USE A FACTOR PALETTE AND NO BINS AND A DIFFERENT TITLE SUBSTRING.
           map_abund_pal = colorFactor(palette = "viridis", 
                                        domain = range_tmp,
                                        na.color = "gray")
           
           rake_lab = paste0("<br>Max: ", data_map3$RAKE_MAX[1])
           
           bins1 = FALSE
           
         }
         
         #FIRST GET BOUNDING BOX.
         bbox1 <- unname(st_bbox(data_map3))
         
         map1 = leaflet() %>% 
             addTiles() %>% 
           fitBounds(bbox1[1], bbox1[2], #SET IT AT CENTER OF BOUNDING BOX
                     bbox1[3], bbox1[4]) %>% 
           addCircleMarkers(data=data_map3$geometry, #ADD MARKERS FOR EACH RAKE SCORE VAL WE HAVE.
                            color = "black",
                            weight = 0.5,
                            radius = 5,
                            opacity = 1,
                            fillColor = map_abund_pal(unlist(as.vector(st_drop_geometry(data_map3[1])))),
                            fillOpacity = 1, 
                            )
         #THEN, PLOT THE LEGEND SLIGHTLY DIFFERENTLY DEPENDING UPON THE FORMAT OF THE RAKE DATA.
         if(bins1 == FALSE) {

           if(data_map3$RAKE_MAX[1] == "Unknown") {

             #Here, we force 0 and 1 to always be in the potential values of the legend no matter what the rake scale was. 
             values_tmp = unique(c(0, 1, sort(unlist(as.vector(st_drop_geometry(data_map3[1]))))))
             labs_tmp = unique(c(0,1, as.character(sort(unique(unlist(as.vector(st_drop_geometry(data_map3[1]))))))))
             
             #THIS IS A BIT HACKY, BUT IF WE ONLY HAVE PRESENCE/ABSENCE DATA, WE CAN CHECK FOR THAT HERE AND PUT BETTER LABELS ON THE LEGEND. 
             if(length(labs_tmp) == 2 && 
                labs_tmp[1] == "0" && 
                labs_tmp[2] == "1") {
               labFormat_custom <- function(type, cuts, p) {
                 c("Not detected", "Detected")
               }
               } else {
                 labFormat_custom <- labelFormat()
               }
           } else {
             values_tmp = as.factor(0:as.numeric(data_map3$RAKE_MAX[1]))
             labs_tmp = as.character(0:as.numeric(data_map3$RAKE_MAX[1]))
             labFormat_custom <- labelFormat()
           }
           
           map1 = map1 %>% 
             addLegend(position = "bottomright", 
                     pal = map_abund_pal, 
                     values = values_tmp,
                     labFormat = labFormat_custom,
                     opacity = 1, 
                     title = HTML(paste0("<span class=italicize>",
                                         gsub(" ", "<br>", taxon_name), 
                                         "</span>", rake_lab))
                     
                     
                     )
         } else {
           map1 = map1 %>% 
             addLegend(position = "bottomright", 
                    pal = map_abund_pal, 
                    values = sort(unlist(as.vector(st_drop_geometry(data_map3[1])))), 
                    bins = seq(from = min(data_map3$taxon), 
                               to = max(data_map3$taxon), 
                               length = 5),
                    opacity = 1, 
                    title = HTML(paste0("<span class=italicize>",
                                        gsub(" ", "<br>", taxon_name), 
                                        "</span>", rake_lab))
                    
                    
           )
         }
         map4download = reactive({ map1 }) #SET THIS AS A REACTIVE.
         
         #USE IN THE DOWNLOAD HANDLER
         output$abundance_map_download = downloadHandler(
           filename = function() {
             paste0("PICharter_", Sys.Date(), ".html") 
           }, 
           content = function(file) {

             saveWidget(map4download(), file) #REFERENCE THE REACTIVE MADE AT THE SAME TIME AS THE MAP BEING RENDERED.
           }
         )
         
         map1 #NEEDED--MUST BE LAST.
           
       })

       tags$figure(
         tags$figcaption("Map of abundance for the taxon and survey date selected. Consult the legend in the map\'s bottom-right corner for the abundance ratings used. The raw survey data are available in a table on the \"Raw survey data\" tab. Lake-level frequency of occurence data are available in a table on the \"Littoral Frequency of Occurrence\" tab.",
                         id = "records_map"),
       leafletOutput("abundance_map")
       )
       
       })

} 

# Observers ---------------------------------------------------------------

#OBSERVER TO (UN)HIDE THE SURVEY SELECTOR AND POPULATE ITS CHOICES
  observeEvent(input$records_dows, {

    #AS SOON AS THE INPUT EXISTS AND A SELECTION OF LAKE IS MADE...
    if(isTruthy(input$records_dows) &&
       input$records_dows != "No selection") {

      disable("records_surveys") #DISABLE WHILE THE OPERATIONS OCCUR
      output$lakefinder_note = renderUI({}) #RENDER NOTHING AT FIRST
      
      #HIDE THE WAITER IF IT'S STILL ON BECAUSE SOMEONE HAS PICKED A DIFFERENT LAKE FROM HAVING ALREADY HAD ONE SELECTED...
      records_waiter$hide()

      #GET SURVEY OPTIONS BY FINDING THE LAKE RECORDS MATCHING THE DOW SELECTED
      surveys2show = db_path %>% 
        arrow::open_dataset() %>% 
        filter(DOW == input$records_dows) %>% #GRAB JUST THE DOW SELECTED BY THE USER
        select(SURVEY_START) %>% #GRAB THE SURVEYS COLUMN AND MAKE UNIQUE
        distinct() %>% 
        dplyr::collect() %>% 
        pull()

      #CONVERT TO DATE, SORT.
      surveys2show = format(
        sort(
          as.Date(surveys2show, format = "%Y-%m-%d"),
          decreasing = TRUE), #PUT MOST RECENT FIRST.
        format = "%Y-%m-%d")
      surveys2show_date = format(
        sort(
          as.Date(surveys2show, format = "%Y-%m-%d"),
          decreasing = TRUE), #PUT MOST RECENT FIRST.
      format = "%m/%d/%Y")

      #FORMAT THE NAMES IN THE SELECTOR TO LOOK "PRETTIER"
      surveys2show_date[stringr::str_sub(surveys2show_date, 1, 1) == "0"] = 
        stringr::str_sub(surveys2show_date[ #THIS BIT OMITS THE LEADING 0 ON MONTHS WITH ONE.
          stringr::str_sub(surveys2show_date, 1, 1) == "0"], start = 2)
    
      names(surveys2show) = surveys2show_date

      #UPDATE THE SELECTOR'S CHOICES
      shinyWidgets::updatePickerInput(session, 
                                      "records_surveys",
                           choices = surveys2show)


      shinyjs::show("records_surveys", anim = T, animType = "slide") #SHOW THE INPUT W/ANIMATION
      enable("records_surveys") #ENABLE ONCE THE OPERATIONS ARE DONE

    } else {

      shinyjs::hide("records_surveys", anim = T, animType = "slide") #HIDE THE INPUT W/ANIMATION

      #RESET THE INPUT
      shinyWidgets::updatePickerInput(session, 
                                      "records_surveys",
                           choices = "Select a lake first")
      
      #HIDE THE WAITER IF IT'S STILL ON BECAUSE SOMEONE HAS PICKED A DIFFERENT LAKE FROM HAVING ALREADY HAD ONE SELECTED...
      records_waiter$hide()
      
      output$lakefinder_note = renderUI({
        div(HTML("If you're unsure of the DOW for the lake you're interested in, <a target= '_blank', href='https://maps1.dnr.state.mn.us/lakefinder/mobile/'>use LakeFinder to get it.</a>"),
            id="lakefinder_note")
      })

    }
  })
  
#OBSERVER WATCHING THE SURVEY SELECTOR AND GRABBING THE DATA
observeEvent(surveys_debounced(), {

    if(isTruthy(input$records_surveys) &&
       !"Select a lake first" %in% input$records_surveys) {
      
      #TURN ON THE WAITER.
      records_waiter$show()
      
      #DISABLE THE DOW SELECTOR IF THIS SELECTOR IS NOT NO SELECTION
      shinyjs::disable("records_dows")
      
      #FIND THE SELECTED LAKE AND THE SELECTED SURVEY(S)
      dataOpened = db_path %>% 
        arrow::open_dataset() %>% 
        filter(DOW == input$records_dows,
               SURVEY_START %in% input$records_surveys) %>%
        dplyr::collect()

      dataOpened = records_tabs_preprocessing(dataOpened) #Pre-process--see decluttering functions.
   
      ###MAKING THE LITTORAL FREQUENCY OF OCCURRENCE SUBTAB!!
      dataLFOO = records_LFOO_subtab(dataOpened)
      
      shinyjs::show("rawTableCaptionDiv", anim = T, animType = "fade") #MAKE THIS CAPTION AREA VISIBLE.
      
      #RENDER TABLE OF SURVEY FILE'S CONTENTS
      output$survey_table = renderDT({
        
      table2show = datatable(dataOpened,
                  fillContainer = TRUE,
                  rownames = FALSE, #TO NOT HAVE EMPTY HEADER ROWS WHICH SCREEN READERS DON'T LIKE.
                  escape = FALSE,
                  selection = 'none',
                  options = base::list(
                    language = list(search = "Search survey results: ",
                                    paginate = list(
                                      "next" = "Next table page",
                                      "previous" = "Previous table page"
                                    )),
                    saveState = TRUE,
                    scrollX = TRUE,
                    scrollY = FALSE,
                    processing = FALSE),
                  #TYPICAL DT TABLE SCREEN READER FIXES.
                  callback = DT_A11Y_Callback
                  ) 
                  
    htmlwidgets::onRender(table2show, "
  function(el) {
    el.setAttribute('aria-describedby', 'raw-table-caption');
  }
")
        
      })
      
      #RENDER THE UI FOR THE TABLE'S CAPTION
      output$rawTableCaptionDiv = renderUI({
        tags$div(
          id = "raw-table-caption",
          role = "caption",
          `aria-label` = "Raw survey data table summary and download options",
          style = "width: 100%;",
          tags$p("This table contains raw data for the survey(s) you selected, as they appear in our database. You can:"),
          tags$ul(
            tags$li(downloadButton("download_records", 
                                   label = "Download these data as a .CSV file.", 
                                   class = "link-style-btn", 
                                   style = "padding: 0; border: none; background: none; cursor: pointer;")),
            tags$li(downloadButton("download_directory", 
                                   label = "Download a .CSV file explaining each column name.", 
                                   class = "link-style-btn", 
                                   style = "padding: 0; border: none; background: none; cursor: pointer;"))
          )
        )
      })

      ###MAKING THE RAKE SCORES SUBTAB!!

      if(all(c("latitude", "longitude") %in% names(dataOpened))) {
        
        #It's possible users will have selected multiple surveys from the current lake, some of which do and some of which don't have spatial data. We should filter here to just those with spatial data for the abundance tab processes specifically. 
        dataJustSpatial = dataOpened %>% 
          filter(!is.na(latitude),
                 !is.na(longitude))
        
        #CUSTOM JS THAT ENABLES THIS TAB, GIVEN THAT SPATIAL DATA EXIST.
      runjs('
      $("#records_outputs li a[data-value=\'2\']").parent().removeClass("disabled");
      
       $("#records_outputs li a[data-value=\'2\']").attr("data-toggle", "tab");
      ')
        
        #POPULATE THE TWO ABUNDANCE SUB-TAB SELECTORS, THEN RENDER THE MAP IN AN INVALIDATION CHAIN.
      records_rakes_subtab_selector1(dataJustSpatial)

      } else {
        #OTHERWISE, DISABLE AND LOCK THIS TAB (NO LOCATION DATA TO MAP)
        runjs('
      // Disable Tab 2
      $("#records_outputs li a[data-value=\'2\']").parent().addClass("disabled");
      
      // Prevent clicking on the disabled tab
      $("#records_outputs li.disabled a").attr("data-toggle", "");
    ')
        updateTabsetPanel(session, "records_outputs", selected = "1") #Flop away from the abundance tab, if that's what folks were on.
        
      }

      records_waiter$hide()

      #OTHERWISE, DISPLAY NOTHING
    } else {

      shinyjs::hide("rawTableCaptionDiv", anim = T, animType = "fade")
      shinyjs::enable("records_dows") #REOPEN THE DOWS SELECTOR
      
      output$survey_table <- renderDataTable({ })
      
      output$survey_metadata <- renderUI({ })
      
      records_waiter$hide()
    }
  })

#OBSERVER THAT WATCHES THE CHECKBOX INPUT AND, IF CHECKED, FILTERS RECORDS TO ONLY THOSE CONTAINING SOME LOCATION DATA.
observeEvent(input$only_spatial, {
  
  #DISABLE ANY WAITERS, AS THEY HAVE A PESKY HABIT OF COMING ON WHEN UNWANTED.
  records_waiter$hide()
  
  #WIPE OUT ANY SELECTED SPECIFIC SURVEY(S), WHICH HAS THE CONSEQUENCE OF ESSENTIALLY RESETTING CERTAIN ASPECTS OF THE TAB
  shinyWidgets::updatePickerInput(session, "records_surveys", selected = "Select a lake first")
  
  #MAKE SURE TO RE-ENABLE THE DOW SELECTOR, IN CASE IT IS CURRENTLY LOCKED.
  shinyjs::enable("records_dows")
  
  #WIPE OUT THE CONTENTS OF THE SUBTABS.
  output$survey_metadata <- renderUI({ })
  output$abundance_selectors <- renderUI({ })
  output$abundance_data <- renderUI({ })
  output$survey_table <- renderDataTable({ })
  shinyjs::hide("records_abund_fieldset")
  shinyjs::hide("rawTableCaptionDiv")
  output$abundance_map <- renderLeaflet({ })
  
  #RESTRICT TO ONLY SPATIAL DATA.
  if(input$only_spatial == TRUE) {
    
    dowslistreduced = dowslist2use[dowslist2use %in% spatial_dows] #FILTER TO THOSE LAKES IN SPATIAL DOWS.
    
    #ENSURE THE INPUT IS POPULATED EITHER WAY.
    updateSelectizeInput(session, "records_dows", 
                         server = T,
                         choices = c("No selection", dowslistreduced),
                         options = list(maxOptions = length(dowslistreduced),
                                        render = I("{
      option: function(item, escape) {
        return '<div class=\"option\" role=\"option\" data-selectable data-value=\"' + escape(item.value) + '\">' + escape(item.label) + '</div>';
      }
    }")))
  } else {
    
    updateSelectizeInput(session, "records_dows", 
                         server = T,
                         choices = c("No selection", dowslist2use),
                         options = list(maxOptions = length(dowslist2use),
                                        render = I("{
      option: function(item, escape) {
        return '<div class=\"option\" role=\"option\" data-selectable data-value=\"' + escape(item.value) + '\">' + escape(item.label) + '</div>';
      }
    }")))
    
  }
})

#THIS OBSERVER WATCHES THE SURVEY TABLE'S SEARCH BAR AND REPORTS TO SCREEN READER USERS ONLY THE NUMBER OF SEARCH RESULTS GENERATED.
#THIS OBSERVER WATCHES THE TABLE AND REPORTS TO SCREEN READER USERS ANY STATE CHANGES TO PAGE/LENGTH/RESULTS.
table_state2 = reactive({
  req(input$survey_table_rows_all)
  req(input$survey_table_state)
  
  list(
    n_matches = length(input$survey_table_rows_all),
    start = input$survey_table_state$start,
    length = input$survey_table_state$length
  )
}) %>% debounce(750)

observe({
  state = table_state2()
  page = floor(state$start / state$length) + 1
  total_pages = ceiling(state$n_matches / state$length)
  
  output$search_record_feedback <- renderUI({
    HTML(paste0(
      "<p>",
      "Showing page ", page, " of ", total_pages, ". ",
      state$n_matches, " result", ifelse(state$n_matches == 1, "", "s"), " total.",
      "</p>"
    ))
  })
})

# Observers related to the abundance map subtab ---------------------------

observeEvent(list(input$abundance_survey, surveys_debounced()), { #This needs to get triggered if users mess around with the surveys they've selected at all. 
  
  if(isTruthy(input$abundance_survey)) {
    records_rakes_subtab_selector2(records_reactives$avail_data)
  }
  
})

observeEvent(input$abundance_taxon, {
  
  if(isTruthy(input$abundance_taxon)) {
    records_rakes_subtab_map(records_reactives$processed_dat)
  }
    
    if("Select a lake first" %in% input$records_surveys) {
      #WHEN THE USER HAS RE-SELECTED NO SELECTION FOR RECORDS_SURVEYS, LET'S INSTEAD WIPE THE MAP BACK OUT. 
      output$abundance_selectors <- renderUI({ })
      output$abundance_data <- renderUI({ })
      shinyjs::hide("records_abund_fieldset")
    }
  
})

#WATCHING THE DOWNLOAD RECORDS BUTTON, HANDLES THE DOWNLOAD PROCESS OF REDACTED DATA

  output$download_records <- downloadHandler(
    filename = function() {
      paste0("PICharterRecord_", input$records_dows, "_", input$records_surveys, ".csv")
    },
    content = function(file) {
      write.csv(records_reactives$avail_data, file, row.names = FALSE)
    }
    
  )
  
  #WATCHING THE DOWNLOAD DIRECTORY BUTTON, HANDLES THE DOWNLOAD PROCESS OF REDACTED DATA
  
  output$download_directory <- downloadHandler(
    filename = function() {
      "PICharterDirectory.csv"
    },
    content = function(file) {
      write.csv(fieldNameDirectory, file, row.names = FALSE)
    }
    
  )
  
  
  #OBSERVER FOR SHOWING/HIDING THE DISCLAIMERS SECTION
  observeEvent(input$records_disclaimers, {
    
    if(checkIfEven(input$records_disclaimers)) {
      
      shinyjs::hide("records_disclaimers_div", 
                    anim = TRUE, 
                    animType = "slide")
      
      updateActionButton(session,
                         "records_disclaimers",
                         label = "  Show disclaimers",
                         icon = icon("plus"))
      
      runjs(paste0("$('#records_disclaimers').attr('aria-expanded', 'false');"))
      
    } else {
      
      shinyjs::show("records_disclaimers_div", 
                    anim = TRUE, 
                    animType = "slide")
      
      updateActionButton(session,
                         "records_disclaimers",
                         label = "  Hide disclaimers",
                         icon = icon("minus"))
      
      runjs(paste0("$('#records_disclaimers').attr('aria-expanded', 'true');"))
    }
  })
  
  
  #OBSERVER FOR SHOWING/HIDING THE LFOO EXPLANATION SECTION
  observeEvent(input$LFOO_explanation, {
    
    if(checkIfEven(input$LFOO_explanation)) {
      
      shinyjs::hide("LFOO_explanation_div", 
                    anim = TRUE, 
                    animType = "slide")
      
      updateActionButton(session,
                         "LFOO_explanation",
                         label = '  Define LFOO',
                         icon = icon("plus"))
      
    } else {
      
      shinyjs::show("LFOO_explanation_div", 
                    anim = TRUE, 
                    animType = "slide")
      
      updateActionButton(session,
                         "LFOO_explanation",
                         label = '  Hide LFOO definition',
                         icon = icon("minus"))
    }
  })
  
  
  #OBSERVER FOR SHOWING/HIDING THE GRAPH FEATURES SECTION
  observeEvent(input$graph_features, {
    
    if(checkIfEven(input$graph_features)) {
      
      shinyjs::hide("graph_features_div", 
                    anim = TRUE, 
                    animType = "slide")
      
      updateActionButton(session,
                         "graph_features",
                         label ='   Graph instructions',
                         icon = icon("plus"))
      
    } else {
      
      shinyjs::show("graph_features_div", 
                    anim = TRUE, 
                    animType = "slide")
      
      updateActionButton(session,
                         "graph_features",
                         label = '  Hide graph instructions',
                         icon = icon("minus"))
    }
  })
  
}