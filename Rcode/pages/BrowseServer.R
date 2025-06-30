browseServer = function(input, output, session) {

  #Establishing any reactive values.
  browse_reactives = shiny::reactiveValues(
    activeIDs = base::character(), #Do we have any active records selected on the browsing tab? At the outset, no, we won't.
    map_curr_df = lakes.summary.sf, #For the interactive map, what data frame should it be pointing to at this moment? Useful for when we filter via the selectors...
    trip_on_clear = stats::rnorm(1), #Just need a random value that changes every time we clear the map so that the observer checking map markers is forced to reconsider upon clearing.
    curr_browseDT = lakes.summary.sf[0,], #For the lake and taxon modals, what data table should we be pulling the records from? To start with, the base one but with no rows in it. 
    curr_taxa_selectors = c("All", db_allSearchableTaxa), #What should the contents of the taxa selector be right now, given appropriate filtering and the common/scientific name toggle?
    last_lake_highlighted = NULL #What was the last lake marker highlighted via row selection?
  )
  
  #Make waiter reactive objects so that these can be deployed for the map and table on the browse tab as the data to be shown is processed. 
  map_waiter <- Waiter$new(id = "interactivemap", 
                           html = spin_hexdots(),
                           color = "#ffb71e",
                           hide_on_render = TRUE)
  table_waiter <- Waiter$new(id = "heresTheTable", 
                             html = spin_wave(),
                             color = "#ffb71e",
                             hide_on_render = TRUE)
  
  
  # Browse the database tab static elements ---------------------------------
  
  # The most basic element on this tab is the download button, which allows users to grab a csv of the current table.
  output$download_table <- downloadHandler("PICharterSummaryRecords.csv",  #File name
                                           contentType = "text/csv", #File type to create
                                           content = function(file) { #Function that produces a file. Because it's a function, I can do some data prep inside.
                                             
                                             data = isolate(browse_reactives$curr_browseDT) #Grab current data.
                                             data = data %>% #Rework the taxa column to remove all HTML tags (thanks CHATGPT!)
                                               mutate(`All taxa` = gsub("<br>", ", ",`All taxa (invasives in bold)`)) %>% #Remove the brs first and replace with commas, then wipe out all remaining tags with just nothing. 
                                               mutate(`All taxa` = gsub("<[^>]*>",
                                                                        "", 
                                                                        `All taxa`)) %>% 
                                               dplyr::select(-`All taxa (invasives in bold)`)
                                             
                                             write.csv(data, file, row.names = FALSE)
                                           })
  
  #The most important of these in some ways is the Leaflet map that displays a circle marker for every lake for which our database has at least one record. This code builds the "initial" version of the map upon load, which we will update using LeafletProxy in several ways. 
  output$interactivemap = leaflet::renderLeaflet({
    
    map_waiter$show()
    curr.df = shiny::isolate(browse_reactives$map_curr_df) #Do not invalidate the map based on whether the data object is being filtered.
    
    curr.df$colors4markers = "#39568CFF"
    curr.df$colors4markers[curr.df$DOW %in% tribal_conflicts] = "#7f7f7f" #gray LAKES ARE THOSE ON TRIBAL LANDS.

    #FOR TRIBAL LAKES, APPEND THE FOLLOWING CLARIFICATION STRING
    curr.df$maplabels[curr.df$DOW %in% tribal_conflicts] = 
      paste0(curr.df$maplabels[curr.df$DOW %in% tribal_conflicts],
             "<br>[Record hidden;<br>on tribal lands.<br>See 'Disclaimers.']")
    
    #Build the map.
    l = leaflet::leaflet() %>%
      #We set a particular min and max zoom so people don't get lost.
      leaflet::addTiles(options = leaflet::providerTileOptions(minZoom = 6, maxZoom = 15)) %>% 
      leaflet::addPolygons(data = MN$geometry, #Add a polygon of the state's borders, colored solid black, size 2.
                  color = "black", 
                  opacity = 1, 
                  fillOpacity = 0, 
                  weight = 2, 
                  group = "MN_boundary") %>% 
      leaflet::addCircleMarkers(data=curr.df$geometry,
                                radius = 4.5, 
                                group = "circ_marks",
                       stroke=T, 
                       color = "black",
                       weight = 2,
                       fillColor = curr.df$colors4markers,
                       fillOpacity = 1, 
                       label = base::lapply(curr.df$maplabels, HTML), 
                       labelOptions = leaflet::labelOptions(
                         className = "map_hovers",
                       ),
                       layerId = curr.df$DOW) #We give each marker an ID unique to its lake's DOW for use downstream.

    session$sendCustomMessage("scrubLeafletMap", list())
    
    l
    
  })
  
  #Because the DOW selector on this tab has so many potential entries, we have to use server-side selectizing to populate it and have it show every potential record. 
  shiny::observeEvent(input$filter_dow, once = TRUE, { #I think the once = TRUE here will get this to trigger only upon startup and all other cases should already be properly handled, but this should be confirmed. This may be an inelegant solution here. 
    shiny::updateSelectizeInput(session, 
                       "filter_dow", 
                       choices = base::c("All", db_allDOWs), 
                       server=TRUE,
                       #THE FOLLOWING CODE ENSURES AFTER THE FACT THAT EVERY OPTION BE GIVEN AN OPTION ROLE FOR ASSISTIVE TECHNOLOGIES AND ALSO HAS AN ESCAPE COMMAND TO ENSURE AN OPTION NEEDN'T BE PICKED (I THINK).
                       options = base::list(maxOptions = base::length(db_allDOWs),
                                              render = I("{
      option: function(item, escape) {
        return '<div class=\"option\" role=\"option\" data-selectable data-value=\"' + escape(item.value) + '\">' + escape(item.label) + '</div>';
      }
    }")
                                            ))
  })
  
  ## Browse the database tab observers ---------------------------------------
  
  #The first observer is the one that tracks if a user clicks any map markers. If they do, we want to add the lakes associated with those map markers to an "actives" list using the IDs contained within the markers (unless the marker is on tribal lands, in which case we ignore the click event). We can then color those markers differently to show that they are active without redrawing any other markers in the process. 
  
  shiny::observeEvent(input$interactivemap_marker_click, priority = 1, {
    
    #Shorthands for convenience
    ID = input$interactivemap_marker_click$id
    actives = browse_reactives$activeIDs #We want to know what was already active prior to any changes. See below.
    
    #See if this ID coming in is already active or not. If not, add it to the actives. If so, though, instead deactivate it.  Bypass this entirely if this is a DOW on the tribal boundaries list though.
    if(!ID %in% tribal_conflicts) {
    if(ID %in% actives) {
      browse_reactives$activeIDs = actives[actives != ID]
    } else {
      browse_reactives$activeIDs = base::c(ID, actives)
    }

  }
    
  })
  
  
  #We then have a second observer also tracking marker clicks, albeit with a lower priority so it resolves second. This one actually renders the new map markers colors-wise, depending upon who is now active.
  
  #We're actually going to fold in the functionality of the "Select_all" button here too, such that clicking that will trigger this observer as well. We just set its priority low so that the updates to the activeIDs list occur first.
  listen_select_click = shiny::reactive({
    base::list(input$interactivemap_marker_click, 
         input$selectAll, 
         browse_reactives$trip_on_clear)
  })
  
  shiny::observeEvent(listen_select_click(), priority = 0, ignoreInit = T, {

    IDs = browse_reactives$activeIDs #STASH CURRENTLY ACTIVE IDS

    #If someone selects all markers and then clears all markers without having clicked on any other inputs or markers, IDs will have length 0, so we need an if/else structure here to prevent an error. 
    if(isTruthy(IDs)) {
      #Counter-intuitively, we need to filter from the entire dataset here because otherwise we lose lakes that are active in other counties, e.g., when then selecting additional lakes in a second county, so we need a wider "scope" when activating and deactivating map markers. 
    filtered.dat = lakes.summary.sf %>%
      dplyr::filter(DOW %in% IDs)  #Get any and all relevant IDs, regardless of how they are relevant.
    } else {
      #WITH THE NEW MATCH ALL TAXA FUNCTIONALITY, WE CAN ALSO END UP HERE WHENEVER USERS TRY TO SELECT ALL MARKERS WHEN NONE ARE SHOWING DUE TO OVERFILTERING. IN THAT CASE, THOUGH, IDs WILL HAVE LENGTH 1 AND EQUAL "", SO WE CAN CHECK FOR THAT INSTANCE HERE.
      if(length(IDs) > 0 && IDs == "") { #THE DOUBLE CHECK HERE IS NECESSARY BECAUSE, OTHERWISE, WE'LL ERROR THIS CHECK IF THE LENGTH OF IDs IS 0. 
        filtered.dat = lakes.summary.sf[0,] #PULL ZERO ROWS.
      } else {
      filtered.dat = browse_reactives$map_curr_df #PULL EVERYTHING, TO HANDLE THE SPECIFIC PRE-IF SITUATION ABOVE
      }
    }

    #WE PAUSE HERE TO ASK--ARE THERE ANY ACTIVE MARKERS THAT WOULD VIOLATE THE CURRENT FILTERING RULES AND THUS WOULDN'T OTHERWISE BE VISIBLE? IF SO, WE SHOULD PROVIDE A WARNING TO THE USER.
    if(isTruthy(browse_reactives$activeIDs) && #THERE NEED TO BE SOME ACTIVE IDs
       all(!is.null(input$filter_taxa), !is.null(input$filter_dow), !is.null(input$filter_county)) && #DON'T BOTHER IF THE REASON IS THAT THEY'VE EMPTIED OUT ONE OF THE SELECTORS. 
       any(!browse_reactives$activeIDs %in% browse_reactives$map_curr_df$DOW)) {
      
      output$active_filters_conflict = renderText({
        HTML("<br><strong>Warning: At least one activated (green) marker on the map doesn't satisfy your current filter selections.</strong>")
        })
      
    } else {
      
      output$active_filters_conflict = renderText({})
      
    }
    
    #To handle both activation and deactivation, we will assume all markers are inactive (blue) and make only those we know are active green. Keep tribal lakes gray.
    filtered.dat$colors4markers = "#39568CFF"
    filtered.dat$colors4markers[filtered.dat$DOW %in% browse_reactives$activeIDs] = "#73D055FF"
    filtered.dat$colors4markers[filtered.dat$DOW %in% tribal_conflicts] = "#7f7f7f"
    
    #FOR TRIBAL LAKES, APPEND THE FOLLOWING CLARIFICATION STRING
    filtered.dat$maplabels[filtered.dat$DOW %in% tribal_conflicts] = 
      paste0(filtered.dat$maplabels[filtered.dat$DOW %in% tribal_conflicts],
             "<br>[Record hidden;<br>on tribal lands.<br>See 'Disclaimers.']")

    #DUE TO THE NEW MATCH ALL TAXA FUNCTIONALITY, filtered.dat MIGHT HAVE ZERO ROWS HERE, IN WHICH CASE NOTHING SHOULD HAPPEN, SO WE CAN PREVENT THAT USING AN IF CHECK HERE. 
    
    if(nrow(filtered.dat) > 0) {
    
    #Update the map using leafletProxy, removing the old marker and replacing it with a new one of the appropriate color. We also leave on whatever markers were already there in whatever colors they already were. 
    l = leaflet::leafletProxy("interactivemap") %>% 
      leaflet::removeMarker(layerId = c(IDs, 
                                        input$interactivemap_marker_click$id))       %>%  #REMOVE THE CLICKED POINT NO MATTER WHAT--IT'LL GET ADDED BACK IF IT SHOULD BE ACTIVE.

      leaflet::addCircleMarkers(data=filtered.dat$geometry, 
                                radius = 4.5, group = "circ_marks",
                       stroke=T, color = "black", weight = 2,
                       fillColor = filtered.dat$colors4markers, fillOpacity = 1, 
                       label = base::lapply(filtered.dat$maplabels, HTML), 
                       labelOptions = leaflet::labelOptions( #See here for details on this--> :https://rstudio.github.io/leaflet/popups.html
                         className = "map_hovers", 
                       ),
                       layerId = filtered.dat$DOW)
      
    session$sendCustomMessage("scrubLeafletMap", list())
      
      l
      
    }

  })
  
  
  
  #The map also needs to update whenever one of the selectors on this tab causes the data the map references to be shrunk or expanded. However, if there are any active markers, we keep those visible and colored appropriately. 
  
  shiny::observeEvent(browse_reactives$map_curr_df, {
    
    shiny::req(input$filter_dow) #This had to be added to prevent bugs with the server-side selectize, since as the "change-over" happens, this input briefly disappears

    map_waiter$show() #Turn on a waiter in case this takes a while. 
    
    #What data should the map currently be pointing to, based on the selectors?
    filtered.dat1 = browse_reactives$map_curr_df
    filtered.dat1$color = "#39568CFF" #We default all the markers to the "deactivated" color to start with. 
    filtered.dat1$color[filtered.dat1$DOW %in% tribal_conflicts] = "#7f7f7f" #Keep tribal lakes gray.
    
    #Then, we figure out what IDs, if any, are active and we set the marker colors for those to the "active" color. 
    active.dat = lakes.summary.sf %>% 
      dplyr::filter(DOW %in% browse_reactives$activeIDs)
    active.dat$color = "#73D055FF"

    #WE PAUSE HERE TO ASK--ARE THERE ANY ACTIVE MARKERS THAT WOULD VIOLATE THE CURRENT FILTERING RULES AND THUS WOULDN'T OTHERWISE BE VISIBLE? IF SO, WE SHOULD PROVIDE A WARNING TO THE USER.
    if(isTruthy(browse_reactives$activeIDs) && #THERE NEED TO BE SOME ACTIVE IDs
       all(!is.null(input$filter_taxa), !is.null(input$filter_dow), !is.null(input$filter_county)) && #DON'T BOTHER IF THE REASON IS THAT THEY'VE EMPTIED OUT ONE OF THE SELECTORS. 
       any(!browse_reactives$activeIDs %in% filtered.dat1$DOW)) {
      
      output$active_filters_conflict = renderText({"<br>Heads-up: At least one activated (green) marker on the map doesn't satisfy your current filter selections."})
      
    } else {
      
      output$active_filters_conflict = renderText({})
      
    }

    #We then glue both sets of records together by rows, eliminating any duplicates. Because we put the actives "on top," any duplicates will always keep the "active" record (or tribal one) and thus yield a blue marker.
    alldat2map = dplyr::bind_rows(active.dat, filtered.dat1) %>%  
      dplyr::distinct(DOW, .keep_all = TRUE) 
    
    if(nrow(alldat2map) > 0) { #IT'S NOW POSSIBLE TO OVERFILTER SUCH THAT NO MARKERS SHOW USING THE MATCH ALL TAXA OPTION. WE MUST HANDLE THAT HERE. 
    
    #Get the bounding box for the current set of markers the map will update to show. This is to allow panning the camera to encompass that box. However, we also need to the bounds of the original dataset so that we can set those as the max bounds. 
    bounds1 = base::unname(sf::st_bbox(alldat2map))
    
    #Here, we figure out which markers might need to be removed from the map so that we can remove only those, not every possible marker. 
    markers2remove = lakes.summary.sf$DOW[!lakes.summary.sf$DOW %in% alldat2map$DOW]
    
    #Then, we figure out which markers actually need to be added and only redraw those on the map. We'll still pan to the entire range of markers being plotted though.
    currentMarkers = input$interactivemap_marker$id
    markers2add.dat = alldat2map[!alldat2map$DOW %in% currentMarkers, ]
    
    #FOR TRIBAL LAKES, APPEND THE FOLLOWING CLARIFICATION STRING
    markers2add.dat$maplabels[markers2add.dat$DOW %in% tribal_conflicts] = 
      paste0(markers2add.dat$maplabels[markers2add.dat$DOW %in% tribal_conflicts],
             "<br>[Record hidden;<br>on tribal lands.<br>See 'Disclaimers.']")
    
    #Update the map using leaflet proxy. Remove any previous markers and then plot news ones and pan to them. 
    p = leaflet::leafletProxy("interactivemap") %>% 
      leaflet::removeMarker(markers2remove) %>% 
      leaflet::addCircleMarkers(data=markers2add.dat$geometry, 
                                radius = 4.5, group = "circ_marks",
                       stroke=T, color = "black", weight = 2,
                       fillColor = markers2add.dat$color, fillOpacity = 1, 
                       label = base::lapply(markers2add.dat$maplabels, HTML), 
                       labelOptions = leaflet::labelOptions( 
                         className = "map_hovers",
                       ),
                       layerId = markers2add.dat$DOW) %>% 
      leaflet::flyToBounds(bounds1[1], bounds1[2], 
                  bounds1[3], bounds1[4])
    
    session$sendCustomMessage("scrubLeafletMap", list())
    
    p
    } else { 
      
      #I BELIEVE ALL THAT SHOULD BE NECESSARY HERE IS TO REMOVE ALL THE MARKERS FROM THE MAP WHEN SOMEONE OVERFILTERS. 
     l = leaflet::leafletProxy("interactivemap") %>% 
        leaflet::clearMarkers()
      
     session$sendCustomMessage("scrubLeafletMap", list())
      
      l
      
      } 
    
    map_waiter$hide() #Turn off the waiter either way. 
  })
  
  
  #The map is updated as the selectors cause the data set to be filtered. Here's how the reactive object that holds the data set gets updated as selectors are fiddled with. 
  shiny::observeEvent(list(input$filter_county, input$filter_taxa, input$filter_dow, input$taxon_filter_rule), 
                      ignoreNULL = TRUE, { #WE NEED THIS OBSERVER TO BE RESPONSIVE TO NULLS--IF FOLKS EMPTY OUT ANY OF THE FILTERS AND DON'T PUT SOMETHING BACK, WE SHOULD EFFECTIVELY LOCK ALL THE OTHER FILTERS BY CHANGING THEIR CONTENTS TO JUST ALL. WE CAN DO THAT HERE BY CHECKING FOR ANY NULLS AND REDUCING THE FILTERED DATASET TO NO ROWS. 
                        
    
    if(any(
      is.null(input$filter_county), 
      is.null(input$filter_dow)
      )) {
      browse_reactives$map_curr_df = lakes.summary.sf[0, ]
    } else {

    #Dummy objects to start with initially. If All is selected for any filter, we need to override these accordingly.
    DOWpicked = input$filter_dow
    countypicked = input$filter_county
    taxonpicked = input$filter_taxa
    
    #LET'S DEFAULT TO SHOWING ALL TAXA IF NONE ARE INSTEAD SELECTED.
    if(is.null(taxonpicked) | length(taxonpicked) == 0) {
      taxonpicked = db_allSearchableTaxa
    }

    if(DOWpicked == "All") {
      DOW2filterwith = lakes.summary.sf$DOW
      shinyjs::enable("filter_taxa"); shinyjs::enable("filter_county") #Turn on these filters if we haven't picked a lake.
    } else {
      DOW2filterwith = base::substr(DOWpicked, 1, 8) #This will throw out all characters past the first 8, which will eliminate the lake name I inserted into the selector choices and allow the rest of the string matching to work as needed.
      shinyjs::disable("filter_taxa"); shinyjs::disable("filter_county") #Turn these filters off if we have picked a lake.
    }
    
    if(countypicked == "All") {
      county2filterwith = lakes.summary.sf$cty_name
    } else {
      county2filterwith = countypicked
    }

    if(length(taxonpicked) == length(db_allSearchableTaxa)) { 
      allowblanks = "" #Some surveys return no plants found, so their taxalist is a blank string. We need to display those, but only when every available taxon has been picked.
      taxon2filterwith = taxonpicked #A RELIC THAT IS STILL CONVENIENT.
    } else {

      allowblanks = FALSE #So this value always exists. No records will ever get picked as a result of this value if it's FALSE. 
      taxon2filterwith = taxonpicked
      
      #Handle the case where our toggle button has us on common names (an odd value). We need to flop the common name we're getting from the selector into the scientific name it should correspond with.
      if(!checkIfEven(input$commonVsSci)) {
        taxon2filterwith = replace_names(taxon2filterwith, #SHOULD WORK REGARDLESS OF VECTOR LENGTH
                                         commonsciNameLookup$CommonName,
                                         commonsciNameLookup$sciname)
      }
      
      #Here, we'll check if they picked an "any" taxon. We'll remove the (any) substring so that any record containing that genus name at all will be returned. The same goes for the "spp." We'll get if this is a common name situation.
      #SHOULD NOW WORK REGARDLESS OF VECTOR LENGTH
      if(any(base::grepl("\\(any\\)", taxon2filterwith, ignore.case = T)) ||
         any(base::grepl("spp\\.", taxon2filterwith, ignore.case = T))) {
        taxon2filterwith = base::gsub(" \\(any\\)", "", taxon2filterwith)
        taxon2filterwith = base::gsub(" spp\\.", "", taxon2filterwith)
      }
    }

    #USER TAXON MATCHING
      taxa_lists = strsplit(lakes.summary.definitive$taxalist, ",\\s*") #REDUCE ALL THE TAXALIST DATA TO A LIST OF CHARACTER VECTORS FOR FAST MATCHING
      
      #CREATE VECTOR OF TRUES/FALSES FOR THE MATCH_ANY CONDITION
      match_any = sapply(taxa_lists, function(taxa) {
        any(taxon2filterwith %in% taxa) || ("" %in% taxa && allowblanks == "")
      })
      #CREATE VECTOR OF TRUES/FALSES FOR THE MATCH_ALL CONDITION
      match_all = sapply(taxa_lists, function(taxa) {
        all(taxon2filterwith %in% taxa) || ("" %in% taxa && allowblanks == "")
      })
      
      #SELECT THE FILTER TO USE DEPENDING ON THE USER'S CHOICE
      taxon_filter = if (input$taxon_filter_rule == "Match ANY") { match_any } else { match_all }
    
    #FUSSY--WORK WITH TEMP OBJECTS TO BE ABLE TO USE THE LOGICAL VECTOR CREATED ABOVE FOR FILTERING WITH SELECTED TAXA.
    tmp1 = lakes.summary.sf
    tmp1$taxon_filter = taxon_filter
    
     tmp2 = tmp1 %>% 
      dplyr::filter(DOW %in% DOW2filterwith,
                    cty_name %in% county2filterwith,
                    taxon_filter) %>%  #FILTER BY THE STRAIGHT-FORWARD RULES BASED ON DOW AND COUNTY SELECTIONS AND MY VECTOR OF TRUE/FALSES FROM ABOVE.
       select(-taxon_filter) #DO NOT CARRY THIS COLUMN FORWARD.

    browse_reactives$map_curr_df = tmp2 #STORE THE RESULT.
    
    }

  })
  
  
  #An observer that just messes with the other selectors if specific selectors are messed with. These have low priority so that they fire last. 
  shiny::observeEvent(input$filter_county, priority = -1, ignoreInit = TRUE, {

    ##We now update the other selectors to be more relevant if we've filtered by county or taxa.
    if(input$filter_county != "All") { #Assuming a user has selected a non "All" county...
      tmp1 <- browse_reactives$map_curr_df #Gotta unpack this guy first...
      
      #Make sure that no taxon has already been selected...
      if(any(input$filter_taxa == "All")) {
        #If none has, update the taxon selector to just taxa found so far in that county.

        if(!checkIfEven(input$commonVsSci)) {
          browse_reactives$curr_taxa_selectors <- c("All", replace_names(
            replace_names(process_taxalist(tmp1$taxalist),
                          commonsciNameLookup$sciname,
                          commonsciNameLookup$sciname_selector),
            commonsciNameLookup$sciname,
            commonsciNameLookup$CommonName))
        } else {
          browse_reactives$curr_taxa_selectors <- c("All", process_taxalist(tmp1$taxalist)) #Stash this list
        }
        
        shinyjs::enable("filter_taxa") #And let people select one of them.
      } else {
        shinyjs::disable("filter_taxa") #If, at this point, a county and a taxon are selected, just let them choose a different county and not also a different taxon! Make them go back to All for county first. 
      }
      #Also, no matter what, filter to just the dows that are relevant for the current county +/or taxon mix.
      shiny::updateSelectizeInput(session, "filter_dow",
                           choices = base::c("All", process_dowslakes(tmp1$DOW, tmp1$LAKE_NAME)), 
                           server = TRUE,
                           options = base::list(maxOptions = base::length(db_allDOWs),
                                                  render = I("{
      option: function(item, escape) {
        return '<div class=\"option\" role=\"option\" data-selectable data-value=\"' + escape(item.value) + '\">' + escape(item.label) + '</div>';
      }
    }")
                                                ))
      
    } else { #Meanwhile, if county is all and there isn't already a taxon chosen...
      if(any(input$filter_taxa == "All")) {
        #Reset the taxon list back to the global default.

        if(!checkIfEven(input$commonVsSci)) {
          browse_reactives$curr_taxa_selectors <- c("All", replace_names(db_allSearchableTaxa,
                                                                         commonsciNameLookup$sciname,
                                                                         commonsciNameLookup$CommonName))
        } else {
          browse_reactives$curr_taxa_selectors <- c("All", db_allSearchableTaxa)
        }

      }
      #And do the same with the dow selector.
      shiny::updateSelectizeInput(session, "filter_dow",
                           choices = base::c("All", db_allDOWs), 
                           server = TRUE,
                           options = base::list(maxOptions = base::length(db_allDOWs),
                                                  render = I("{
      option: function(item, escape) {
        return '<div class=\"option\" role=\"option\" data-selectable data-value=\"' + escape(item.value) + '\">' + escape(item.label) + '</div>';
      }
    }")
                                                ))
    }
    
  })

  shiny::observeEvent(list(input$filter_taxa, 
                           input$taxon_filter_rule), #THIS ALSO NEEDS TO TRIGGER WHEN THIS MATCH RULE IS FLOPPED BECAUSE IT MAY AFFECT HOW MANY MARKERS ARE SHOWING AND THUS NEEDS TO FORCE THE OTHER TWO SELECTORS TO UPDATE.
                      priority = -2, ignoreInit = TRUE, {

    if(all(input$filter_taxa != "All")) { #Assuming a user has selected a non "All" taxon and not also all in any capacity...
      tmp1 <- browse_reactives$map_curr_df #Gotta unpack this guy first...
      
      #Make sure that no county has already been selected...
      if(input$filter_county == "All") {
        #If none has, update the county selector to just counties where that taxon has been found.
        shiny::updateSelectInput(session, "filter_county", 
                        choices = base::c("All", base::sort(base::unique(tmp1$cty_name))))
        shinyjs::enable("filter_county") #And let people select one of them.
      } else {
        shinyjs::disable("filter_county") #If, at this point, a county and a taxon are selected, just let them choose a different taxon and not also a different county! Make them go back to All for taxon first. 
      }
      #Also, no matter what, filter to just the dows that are relevant for the current county +/or taxon mix.
      
      #THANKS TO THE NEW TAXON FILTERING RULES, THIS CAN YIELD " ()" WHICH SHOULD BE AVOIDED 
      if(nrow(tmp1) == 0) {
        dow_choices = "All"
      } else {
      dow_choices = base::c("All", process_dowslakes(tmp1$DOW, tmp1$LAKE_NAME))
      }
      
      shiny::updateSelectizeInput(session, "filter_dow",
                           choices = dow_choices, 
                           server = TRUE,
                           options = base::list(maxOptions = base::length(db_allDOWs),
                                                  render = I("{
      option: function(item, escape) {
        return '<div class=\"option\" role=\"option\" data-selectable data-value=\"' + escape(item.value) + '\">' + escape(item.label) + '</div>';
      }
    }"))) #THIS BIT ENSURES THAT EVERY ITEM IN THE LIST GETS THE ARIA ROLE OF OPTION IN CASE IT WASN'T ALREADY.
      
    } else {
      if(input$filter_county == "All") { #Meanwhile, if taxon is all and there isn't already a county chosen...
        #Reset the county list back to the global default.
        shiny::updateSelectInput(session, "filter_county", 
                        choices = base::c("All",db_allCounties))
      }
      #And do the same with the dow selector.
      shiny::updateSelectizeInput(session, "filter_dow",
                           choices = base::c("All", db_allDOWs), 
                           server = TRUE,
                           options = list(maxOptions = base::length(db_allDOWs),
                                                render = I("{
      option: function(item, escape) {
        return '<div class=\"option\" role=\"option\" data-selectable data-value=\"' + 
        escape(item.value) + 
        '\">' + 
        escape(item.label) + 
        '</div>';
      }
    }")
                                                ))
    }
    
  })
  
  
  #An observer that tracks to see whenever browse_reactives$curr_taxa_selectors changes and does an updateSelectInput as a result.
  shiny::observeEvent(browse_reactives$curr_taxa_selectors, {
    
    req(input$filter_taxa) #KEEP THIS FROM RUNNING ON STARTUP WHEN THIS INPUT VAL WILL BE NULL AND ALSO ANY TIME IT HAPPENS TO EMPTY OUT AND BE LEFT EMPTY
    
    #Figure out what taxon was selected before the switch.
    curr_taxon = input$filter_taxa
    
    #If any are "All", keep selected value as all.
    if(any(curr_taxon == "All")) {
      new_selected = "All"
    } else {

      #Otherwise, swap to the equivalent scientific or common name of the same taxon.
    if(curr_taxon[1] %in% commonsciNameLookup$sciname) { #SHOULD ONLY NEED TO CHECK THE FIRST ENTRY HERE TO DISCERN WHICH LIST TO PULL FROM.
      new_selected = sapply(curr_taxon, USE.NAMES = FALSE, function(taxa) {
        commonsciNameLookup$CommonName[taxa == commonsciNameLookup$sciname]
      })
    } else {
      new_selected = sapply(curr_taxon, USE.NAMES = FALSE, function(taxa) {
        commonsciNameLookup$sciname[taxa == commonsciNameLookup$CommonName] 
      })
      }
    }

    #Update the selector to make the switch between naming conventions, keeping the appropriate previous selection. 
    shiny::updateSelectInput(session, "filter_taxa", 
                             choices = browse_reactives$curr_taxa_selectors,
                             selected = new_selected)
    
  })
  
  #An observer that tracks the toggle names button and flops the current taxa in the selector reactive value object accordingly.
  shiny::observeEvent(input$commonVsSci, {
    
    if(!checkIfEven(input$commonVsSci)) {
      browse_reactives$curr_taxa_selectors = base::c("All", base::sort(replace_names(browse_reactives$curr_taxa_selectors,
                                                           commonsciNameLookup$sciname_selector,
                                                           commonsciNameLookup$CommonName)))
    } else {
      browse_reactives$curr_taxa_selectors = base::c("All", base::sort(replace_names(browse_reactives$curr_taxa_selectors,
                                                           commonsciNameLookup$CommonName,
                                                           commonsciNameLookup$sciname_selector
                                                           )))
    }
    
  })

  
  #The other major element on the browse tab is the "records table," a data table that shows more information about the surveys we know about at the lakes we have records for. The table starts out blank. 
  output$interactivemapdf <- DT::renderDT({
    
    #FIRST, FACTOR THE DATA.
    table_data = lakes.summary.definitive %>% 
      filter(FALSE) %>% 
      data.frame() %>% 
      rename(
        "DOW #" = DOW,
        "Lake" = LAKE_NAME, 
        "# years surveyed" = nyears,
        "# surveys" = nsurveys,
        "Survey dates" = surveylist,
        "# taxa observed" = ntaxa,
        "All taxa (invasives in bold)" = taxalist,
        "County" = cty_name
      ) %>% 
      select(`DOW #`, Lake, County, `# surveys`, 
             `Survey dates`, `# taxa observed`, `All taxa (invasives in bold)`)
    
    #THEN, MAKE INTO A DT TABLE USING THE SETTINGS DESIRED. NOTE THAT dataTableProxy requires rownames for some reason. See here --> https://stackoverflow.com/questions/56879672/how-to-replacedata-in-dt-rendered-in-r-shiny-using-the-datatable-function
    datatable_obj <- DT::datatable(
      table_data,
      fillContainer = TRUE,
      escape = FALSE,
      selection = list(mode = "single", target = "row"),
      options = list(
        stateSave = TRUE,
        language = list(emptyTable = HTML("<caption style = 'color: #545454; width: max-content;'>No lakes activated yet.</caption>"),
                        search = "Search lake records: ",
                        paginate = list(
                            "next" = "Next table page",
                            "previous" = "Previous table page"
                          )),
        columnDefs = list(
          list(visible = FALSE, targets = 0), #HIDE ROWNAMES COLUMN FROM THE USER.
          list(className = 'dt-right', targets = "_all"),
          list(className = 'dt-top', targets = "_all"),
          list(width = "200%", targets = 7) #This makes the taxonomic column extra wide, if possible, so taxa generally fit on one line each rather than breaking across 2. 
        ),
        bordered = TRUE,
        scrollX = TRUE,
        paging = TRUE,
        searching = TRUE,
        ordering = TRUE
      ),
      callback = DT_A11Y_Callback
    )
    
    #THEN, ONCE WE RENDER THE DT VERSION ABOVE, ASSOCIATE IT WITH THE CAPTION FOR SCREEN READERS.
    htmlwidgets::onRender(
      datatable_obj,
      "function(el) {
       el.setAttribute('aria-describedby', 'browseTableCaptionDiv');
     }"
    )
    
  })

  #SO THAT WE CAN EMBED THE DOWNLOAD TABLE BUTTON RIGHT INSIDE THE TABLE CAPTION, WHERE IT'S MORE SENSIBLE, WE USE A RENDERUI TO DISPLAY THE CAPTION RIGHT ABOVE THE TABLE RATHER THAN INSIDE IT SO THAT SHINY (NOT DT) HAS CONTROL OVER THE DOWNLOAD HANDLER. WE DON'T USE THE CAPTION ELEMENT HERE BECAUSE, OUTSIDE OF A TABLE, IT'S PESKY. WE INSTEAD USE ARIA BITS AND BOBS TO MAKE THE CONNECTION CLEAR.
  output$browseTableCaption = renderUI({
    tags$div(
      id = "browseTableCaptionDiv",
      role = "caption",
      `aria-label` = "Interactive data table summary and download option",
      style = "width: 100%; margin: 5px;",
      tags$p("Summary of survey data by lake. Click or tap on a lake's map marker to activate it and display its summary data here. You can also ",
             downloadButton("download_table", "download this table as a .CSV file", 
                            class = "link-style-btn",  
                            style = "padding: 0; border: none; background: none; cursor: pointer;"),
             " for use in spreadsheet software.")
    )
  })
  
  observeEvent(input$interactivemapdf_rows_selected, ignoreNULL = FALSE, {

    selected_rows <- input$interactivemapdf_rows_selected #FIND ROW SELECTED
    
    #IF THERE IS ONE...
    if (length(selected_rows) > 0) {
      
      grab_table = browse_reactives$curr_browseDT #GET CURR DF USED FOR TABLE
      DOW1 = grab_table$`DOW #`[selected_rows] #GRAB THE D0W 0F THE SELECTED LAKE
      
      #GET THE MAPPING DF FOR JUST THAT LAKE
      lake2plot = lakes.summary.sf %>% 
        filter(DOW == DOW1)
      
      #IF A PREVIOUS LAKE WAS HIGHLIGHTED...
      if(isTruthy(browse_reactives$last_lake_highlighted)) {
        #GET A MAPPING DF FOR THAT LAKE TOO
        prevlake2plot = lakes.summary.sf %>%
          filter(DOW == browse_reactives$last_lake_highlighted)
        prevDOW = prevlake2plot$DOW
      }
      
      #REDRAW NEWLY HIGHLIGHTED MARKER WITH THICKER OUTLINE AND green
      l = leafletProxy("interactivemap") %>% 
        removeMarker(DOW1) %>% 
        leaflet::addCircleMarkers(data=lake2plot$geometry, 
                                  radius = 4.5, group = "circ_marks",
                                  stroke=T, color = "#FFDB58", weight = 5,
                                  fillColor = "#73D055FF", fillOpacity = 1, 
                                  label = base::lapply(lake2plot$maplabels, HTML), 
                                  labelOptions = leaflet::labelOptions( 
                                    className = "map_hovers",
                                  ),
                                  layerId = lake2plot$DOW)
      
      session$sendCustomMessage("scrubLeafletMap", list())
      
      l
      
      #AS NEEDED, REDRAW PREVIOUSLY HIGHLIGHTED MARKER TO NOT BE HIGHLIGHTED
      if(isTruthy(browse_reactives$last_lake_highlighted)) {
      l = leafletProxy("interactivemap") %>% 
          removeMarker(prevDOW) %>% 
        leaflet::addCircleMarkers(data=prevlake2plot$geometry, 
                                  radius = 4.5, group = "circ_marks",
                                  stroke=T, color = "black", weight = 2,
                                  fillColor = "#73D055FF", fillOpacity = 1, 
                                  label = base::lapply(prevlake2plot$maplabels, HTML), 
                                  labelOptions = leaflet::labelOptions( 
                                    className = "map_hovers",
                                  ),
                                  layerId = prevlake2plot$DOW)
        
      session$sendCustomMessage("scrubLeafletMap", list())
        
        l
      }
    
    browse_reactives$last_lake_highlighted = DOW1 #ADD THE NEWLY HIGHLIGHTED MARKER TO THE TRACKER
    } else {
      
      #BUT IF THERE ARE NO LAKES SELECTED, COULD BE WE'RE DE-SELECTING A SELECTED ROW...
      if(isTruthy(browse_reactives$last_lake_highlighted)) {
        #IF SO, FIND PREV LAKE SELECTED AND DEHIGHLIGHT IT, AS ABOVE
        prevlake2plot = lakes.summary.sf %>% 
          filter(DOW == browse_reactives$last_lake_highlighted)
        prevDOW = prevlake2plot$DOW
        
      l = leafletProxy("interactivemap") %>% 
          removeMarker(prevDOW) %>% 
          leaflet::addCircleMarkers(data=prevlake2plot$geometry, 
                                    radius = 4.5, group = "circ_marks",
                                    stroke=T, color = "black", weight = 2,
                                    fillColor = "#73D055FF", fillOpacity = 1, 
                                    label = base::lapply(prevlake2plot$maplabels, HTML), 
                                    labelOptions = leaflet::labelOptions( 
                                      className = "map_hovers",
                                    ),
                                    layerId = prevlake2plot$DOW)
        
        browse_reactives$last_lake_highlighted = NULL #WIPE OUT THAT STORED DOW.
        
        session$sendCustomMessage("scrubLeafletMap", list())
        
        l
      }
      
    }
  })
  
  #But, as markers are clicked on and their IDs become "active," those records appear in the table. We use a DT proxy structure here to just "update" the table instead of drawing it each time. This has the added bonus that the table will now just exist (blank) at launch, so no one will be confused as to what that div is for. For more details on using the proxy, which works differently than in Leaflet, see here--> https://thatdatatho.com/r-shiny-data-table-proxy-replace-data/
  
  DTproxyBrowse = DT::dataTableProxy("interactivemapdf") #Set up the proxy...
  
  #Listen for both changes in the activeIDs and in the common vs. sci name toggle.
  shiny::observeEvent(base::list(browse_reactives$activeIDs,input$commonVsSci), { 

    #Here, we figure out which records we should be showing and render a table showing those records. We have to do this in a few steps...
    
      #First, we create an initial records table by filtering our complete summary data file down to just activeIDs. 
      create_recordsDT1 = lakes.summary.definitive %>% 
        dplyr::filter(DOW %in% browse_reactives$activeIDs) %>% 
        dplyr::filter(!DOW %in% tribal_conflicts) %>% #EXCLUDE TRIBAL LAKES. A PRECAUTION. I DON'T THINK THIS IS ACTUALLY NEEDED, SINCE THESE SHOULD NOT BE ABLE TO BECOME ACTIVE. 
        base::data.frame() %>% 
        #SORT THE TABLE IN THE ORDER IN WHICH USERS SELECT LAKES
        arrange(factor(DOW, levels = browse_reactives$activeIDs)) %>% 
        dplyr::rename("DOW #" = DOW, #Nicely represent the columns in the data table. 
               "Lake" = LAKE_NAME, 
               "# years surveyed" = nyears,
               "# surveys" = nsurveys,
               "Survey dates" = surveylist,
               "# taxa observed" = ntaxa,
               "All taxa (invasives in bold)" = taxalist,
               "County" = cty_name) %>% 
        #Show only the columns we truly need.
        dplyr::select(`DOW #`, Lake, County, `# surveys`, 
                      `Survey dates`, `# taxa observed`, `All taxa (invasives in bold)`)
      
      #Pause here to consider if any records showing have 0 taxa found. We'll insert some nicer-looking values here but only here so that it doesn't mess up any other checks or alter the state of the original data. 
      create_recordsDT1$`# taxa observed`[create_recordsDT1$`All taxa (invasives in bold)` == ""] = 0
      create_recordsDT1$`All taxa (invasives in bold)`[create_recordsDT1$`All taxa (invasives in bold)` == ""] = "None"

      
      #Pause here to consider whether we should be displaying common or scientific names. If common (not the default, thus any odd number for the button value), use mutate and replace_names() to swap in all the common names for all the scientific ones.
      if(!checkIfEven(input$commonVsSci)) {
        create_recordsDT1 = dplyr::mutate(create_recordsDT1, 
                      `All taxa (invasives in bold)` = replace_names(`All taxa (invasives in bold)`, 
                                                                     commonsciNameLookup$sciname,
                                                                     commonsciNameLookup$CommonName))
      }

        #Here, we force <em> elements around the taxon names and also insert a terminal comma after the string of all names. We also insert line break elements in between every taxon so that they parse onto separate lines for ease of reading. 
      create_recordsDT1 = create_recordsDT1 %>% 
        dplyr::mutate(Lake = stringr::str_to_title(Lake),
               `All taxa (invasives in bold)` = base::paste0("<em>", base::gsub(",", ",<br>", `All taxa (invasives in bold)`), "</em>"), 
               #Here, we do something similar with survey dates so they too parse onto individual lines. 
               `Survey dates` = base::gsub(",", ",<br>", `Survey dates`)) %>%  
        #We then yank the commas back out for a cleaner visual appearance, especially since they were more for "work" (delimiting) than "show" anyway.
        dplyr::mutate(`All taxa (invasives in bold)` = base::gsub(",", "", `All taxa (invasives in bold)`))

      #Again, pause here to consider whether we are displaying common or scientific names (common == odd numbers for the button). This is so we can know which values in the table to be highlighting as invasive. 
      if(!checkIfEven(input$commonVsSci)) {
        column2use = invasives_list$common
      } else {
      column2use = invasives_list$scientific
      }
      
      #We next go in and tack a special span element around every invasive species that shows up in the table so we can give those special styling so they jump out.
      for(i in 1:base::length(column2use)) { #Reference this generic object created above here now throughout. 
        create_recordsDT1$`All taxa (invasives in bold)` = 
          base::gsub(x = create_recordsDT1$`All taxa (invasives in bold)`,
               pattern = column2use[i],
               replacement = base::paste0("<span><strong aria-label = 'Invasive Species'>", column2use[i], "</strong></span>")
          )
      }
      
      #Now we actually make the data.table object to pass to the proxy.
      newBrowseDT = dplyr::tibble(create_recordsDT1)
      
      #We fling this over to the reactives list for this tab so that we can grab it for the lake/taxon modals so we needn't be filtering those a second time over there.
      browse_reactives$curr_browseDT <- newBrowseDT

      DT::replaceData(proxy = DTproxyBrowse, data = newBrowseDT) #Pass the new table to the proxy

  })
  
  #The rest of the observers on this tab have more narrowly defined roles. First, there are the select all/clear all buttons...
  
  #For the clear all button, we need to reset all the selectors to "all." Also, we clear the reactive values object for active IDs so that no IDs are active. Also, make the map once again point to the original summary data frame for records to show. 
  shiny::observeEvent(input$clearActives, {
    
    output$active_filters_conflict = renderText({}) #WIPE THIS WARNING OUT, IF APPLICABLE.

    shiny::updateSelectInput(session, "filter_dow", selected = "All")
    shiny::updateSelectInput(session, "filter_county", selected = "All")
    shiny::updateSelectInput(session, "filter_taxa", choices = browse_reactives$curr_taxa_selectors,
                      selected = "All")
    
    browse_reactives$activeIDs = base::character() #Reset to an empty character vector of no blank values.
    browse_reactives$trip_on_clear = stats::rnorm(1) #This forces the leafletProxy earlier to trip here in case we have selected everything and then cleared everything--nothing otherwise would force that observer to reconsider the map marker colors.
    browse_reactives$map_curr_df = lakes.summary.sf #Reset this table back to the original one.
    browse_reactives$curr_browseDT <- lakes.summary.sf[0,] #And also this one.
    browse_reactives$last_lake_highlighted <- NULL
    
    #And re-render the map to its starting point.
    curr.df = lakes.summary.sf
    curr.df$colors4markers = "#39568CFF"
    curr.df$colors4markers[curr.df$DOW %in% tribal_conflicts] = "#7f7f7f" #gray LAKES ARE THOSE ON TRIBAL LANDS.
    
   l = leaflet::leafletProxy("interactivemap") %>% 
      leaflet::clearMarkers() %>% 
      leaflet::addCircleMarkers(data=curr.df$geometry, radius = 4.5, group = "circ_marks",
                       stroke=T, color = "black", weight = 2,
                       fillColor = curr.df$colors4markers, fillOpacity = 1, 
                       label = base::lapply(curr.df$maplabels, HTML), 
                       labelOptions = leaflet::labelOptions(
                         className = "map_hovers",
                       ),
                       layerId = curr.df$DOW)
    
   session$sendCustomMessage("scrubLeafletMap", list())
    
    l
  })
  
  
  #For the select all button, all we do is turn every marker currently shown into an "active" one, with all the usual consequences.
  
  shiny::observeEvent(input$selectAll, priority = 1, {
    
    #Which DOWs are currently visible on the map?
    DOWs2grab = browse_reactives$map_curr_df$DOW
    
    #Now, exclude tribal lakes
    DOWs2grab = DOWs2grab[!DOWs2grab %in% tribal_conflicts]
    
    #Add all of them to the actives list if they aren't already there. 
    browse_reactives$activeIDs = 
      base::sort(base::unique(base::c(browse_reactives$activeIDs, DOWs2grab)))
  })
  
  #The last two observers on the browse tab relate to the buttons that create modal popups. These popups contain some summary info about the records currently shown in the table at the time of pressing--lists of all lakes and all taxa, cumulatively. These are the first of many outputs stakeholders may want us to provide to them easily.
  
  shiny::observeEvent(input$taxa_list, {
    
    #Grab just the records data plotted in the records table. 
    current_table_dat = browse_reactives$curr_browseDT
    
    #Chop out the relevant taxonomic data into vectors and stick em together all nice-like. 
    taxa2chop = current_table_dat$`All taxa (invasives in bold)`
    taxa2chop = base::gsub("</em>", "", taxa2chop) #Get rid of this for now--we'll put it back later
    taxa2chop = base::gsub("<em>", " ", taxa2chop) #For proper uniqueing. 
    taxa2chop = base::gsub("<br>", "<br>,", taxa2chop) #Give myself commas to break on.
    taxa2chop = taxa2chop[!taxa2chop == " None"] #REMOVE THE NONE ENTRIES FROM THIS

    if(!isTruthy(taxa2chop)) {choppedtaxa2 = "No taxa listed"} else { #For prettier handling of an empty table.
      choppedtaxa2 = base::paste0(taxa2chop, "<br>,") #Add in line breaks between each taxa list in the vector.
    }

    #I believe this pulls the result into a single string from a vector. It also sorts and uniques and trims the final string. We then paste <em> tags around the whole thing to keep things italics.
    choppedtaxa3 = stringr::str_c(
      base::trimws(base::sort(base::unique(base::unlist(
        stringr::str_split(choppedtaxa2, ","))))), collapse = "")
    choppedtaxa3 = base::paste0("<em>", choppedtaxa3, "</em>")
    
    shiny::showModal(
      shiny::modalDialog(fade = TRUE, 
                         size = "m", 
                         easyClose = TRUE, 
                         title = "Taxa list",
                         HTML(base::paste0("<div class = 'infoPopUpsText'>Here's a list of all taxa listed at least once across the records showing in the table (any invasives first): <br><br><em>", choppedtaxa3, "</em></div>"))
                         
      )
    )

  })
  
  
  #This is the lakes list modal instead. 
  shiny::observeEvent(input$lakes_list, {
    
    current_table_dat = browse_reactives$curr_browseDT

    #Chop out the relevant lake identification data into vectors and stick em together all nice-like. 
    DOWs2show = current_table_dat$`DOW #`
    Lakes2show = current_table_dat$Lake
    Counties2show = current_table_dat$County

    #Nicer handling of the case when there are no records in the table. 
    if(!isTruthy(Lakes2show)) { Lakes2show = "No lakes"; Counties2show = "No " } 
    
    #This will stick together, for each lake, a DOW and a nicely formatted lake name (if we have one). It will also insert line breaks after each one. 
    combos2show = base::paste0(DOWs2show, " (", tools::toTitleCase(Lakes2show),  ") -- ", Counties2show, " County<br>")

    string2show = stringr::str_c(combos2show, collapse = "") #Collapse the vector into a string.
    
    shiny::showModal(
      shiny::modalDialog(fade = TRUE, 
                         size = "m", 
                         easyClose = TRUE, 
                         title = "Lakes list",
                         HTML(base::paste0("<div class = 'infoPopUpsText'>Here is a list of all the lakes with records currently showing in the table: <br><br>", string2show, "</div>"))
          
      )
    )

  })
  
  #OBSERVER TO AUTOMATICALLY SELECT A LAKE IF ONE IS PICKED FROM THE FILTER AS THO ITS MARKER HAS BEEN CLICKED BY MAKING IT ACTIVE.
  observeEvent(input$filter_dow, {
 
    #ONLY IF LAKE IS NOT ALL
    if(input$filter_dow != "All" &&
       input$filter_dow != "") { #THIS HAPPENS AT ONE POINT DURING START-UP, AND WE DON'T WANT THAT BLANK STRING IN THERE.
      
      #GET JUST THE DOW FROM THE FILTER
      dow2add = str_sub(input$filter_dow, 1,8)
      
      #UPDATE THE ACTIVE IDS LIST, MAKING SURE TO UNIQUE SO THAT A LAKE CAN'T BE ACTIVE TWICE VIA TWO DIFFERENT MECHANISMS.
      browse_reactives$activeIDs = unique(c(dow2add, browse_reactives$activeIDs))
      
    }
    
  })

  #This is the modal that contains all the "what is this app all about?" content. 
  shiny::observeEvent(list(
    input$browse_info #TRIGGER ON BUTTON PRESS AS NORMAL BUT ALSO ON STARTUP BY VIRTUE OF NOT IGNORING INIT.
    ), 
 {

    shiny::showModal( 
      shiny::modalDialog(
        title = HTML("<h3>Welcome to P.I Charter!<h3>"), #SCREEN READERS NEED A HEADER HERE.
        size = "l",
        easyClose = T,
        div(class = "flexthis flexcol width100", 
            style = "min-width: 330px;",
            #THIS IS A SKIP TO MAIN CONTENT LINK
            tags$a(
              href = "#mainOverviewContent",
              class = "skip-link",
              "Skip to the main content of the Overview tab"
            ),
            actionButton("howto8",
                         class = "howtomodalbuttons",
                         "What is P.I. Charter?",
                         icon = icon("plus"),
                         #ENSURE USE OF AN ACCESSIBLE ICON AND ALSO MAKE SURE ARIA ATTRIBUTES RECOGNIZE THIS AS AN ACCORDION CONTROLLER.
                         `aria-expanded` = "true", #START OPEN!
                         `aria-controls` = "howtoanswer8"),
            div(id = "howtoanswer8", #NOT HIDDEN
                       class = "howtoanswers",
                       HTML("<b>P.I. Charter</b> is an online database for Point-Intercept aquatic plant survey data for the US state of Minnesota, hosted by the <a href='https://maisrc.umn.edu/', target ='_blank'>Minnesota Aquatic Invasive Species Research Center (MAISRC) at the University of Minnesota</a>.<br><br>
                            
                            <b>P.I. Charter</b> is a collaborative effort among ecologists, managers, and contributors compiling records from over a 1,500 P.I. surveys to date from various organizations and surveyors. <a href='https://scholar.google.com/citations?user=-WpMgAQAAAAJ&hl=en', target='_blank'>Dr. Mike Verhoeven</a> painstakingly requested, cleaned, aggregated, and analyzed many of these surveys over many years, and <a href='https://maisrc.umn.edu/alex-bajcz', target='_blank'>Dr. Alex Bajcz</a> has since built this Shiny application to house this database long-term.This app is an acknowledgement of the immense work these survey data represent to collect and an assurance to stakeholders throughout the state that MAISRC will keep aggregating these essential data well into the future!<br><br>
                            
                            <strong>Important: Due to how <b>P.I. Charter</b> is hosted, it unavoidably times out after 15 minutes of inactivity. If this happens, you will need to refresh the page to continue, and all your progress will be lost. Please plan accordingly!</strong>")),
          actionButton("howto1",
                       class = "howtomodalbuttons",
                       "What Can I Do On P.I. Charter?",
                       icon = icon("plus"),
                       `aria-expanded` = "true", #START OPEN!
                       `aria-controls` = "howtoanswer1"),
          div(id = "howtoanswer1", #NOT HIDDEN
                     class = "howtoanswers",
                     HTML("Presently, <b>P.I. Charter</b> has four tabs. On the 'Overview' tab (the landing page), you can explore summaries of the survey records in our database at the lake level; on the 'Leaderboard' tab, you can see everyone who has contributed to this database so far; on the 'Submissions' tab, you can submit new data files to our database; and on the “Records’ tab, you can see the raw data from individual surveys and get survey-level summaries of those data.")), #***Maddie suggested putting links to video tutorials here.
          actionButton("howto0",
                       class = "howtomodalbuttons",
                       "Video Tutorial for the Overview Tab",
                       icon = icon("plus"),
                       `aria-expanded` = "false",
                       `aria-controls` = "howtoanswer0"),
          hidden(div(id = "howtoanswer0",
                     class = "howtoanswers",
                     HTML("You can find a detailed <a href = 'https://www.youtube.com/watch?v=7SpdcWgZ0ww' target = '_blank'>video tutorial covering the uses of the Overview tab</a> of <b>P.I. Charter</b> on MAISRC\'s Youtube channel (closed captioning available; link opens in a new tab). A text summary is also available in the next menu below."))),
          actionButton("howto2",
                       class = "howtomodalbuttons",
                       "Guide to the Overview Tab",
                       icon = icon("plus"),
                       `aria-expanded` = "false",
                       `aria-controls` = "howtoanswer2"),
          hidden(div(id = "howtoanswer2",
                     class = "howtoanswers",
                     HTML("On the left side of the Overview tab, you can find a map of Minnesota. The markers on the map show lakes that have P.I. survey records in our database so far. Click or tap on any marker to select and activate it. When you do, its corresponding records will appear in the table on the right side of the tab. The table is sortable, filterable, and paginated so that you can explore as many lakes' records at a time as you want.<br><br>

Open up the \'Show filters and toggles\' menu to see more options for engaging with the map and table; for example, you can clear all selected records by using the \'Clear selections\' toggle or clear just one selected marker by clicking or tapping a map marker a second time. You can select all the markers currently shown on the map by hitting the \'Select all markers\' button.<br><br>

If you would like to more easily find a specific lake, use the \'DOW (Lake)\' filter. If you want to see only markers from a specific county, use the \'County\' filter. If you want to see which lakes a specific taxon has been found in so far, use the \'Taxa observed\' filter. Note that only a small fraction of all Minnesota lakes have been surveyed. As such, keep in mind that a specific taxon may exist at more lakes than is shown. Lastly, the \'Toggle names\' button will switch between scientific and common names for all taxa on this tab. For more details on any aspect of this tab, see the video tutorial linked to in the menu above this one."))),
          actionButton("howto3",
                       class = "howtomodalbuttons",
                       "What Is A P.I. Survey?",
                       icon = icon("plus"),
                       `aria-expanded` = "false",
                       `aria-controls` = "howtoanswer3"),
          hidden(div(id = "howtoanswer3",
                     class = "howtoanswers",
                     HTML("A P.I. survey is a systematic exploration of a lake's aquatic plant community. Here's a very quick description of how such a survey is generally performed: First, a grid is laid over a map of a lake; sampling locations become the intersections in that grid. To collect a sample, a metal rake head tied to a pole or rope is dragged along the lake bottom. All taxa attached to the rake head are identified and their relative abundances are recorded. After dozens or even hundreds of rake tosses, a picture emerges about the diversity and abundance of plant taxa in a lake! If you want to know more about survey methods, <a href='https://www3.uwsp.edu/cnr-ap/UWEXLakes/Documents/ecology/Aquatic%20Plants/PI-Protocol-2010.pdf', target = '_blank'>consider checking out this PDF document detailing the process used by the Wisconsin DNR</a>, which uses a very similar approach to the one generally used in Minnesota. Additionally, <a href='https://files.dnr.state.mn.us/eco/lake-habitat/lake-plant-survey-manual.pdf', target='_blank'>Chapter 5 of this PDF document</a> that describes the process used in Minnesota in more detail."))),
          actionButton("howto4",
                       class = "howtomodalbuttons",
                       "How are P.I. Surveys Different From Other Kinds?",
                       icon = icon("plus"),
                       `aria-expanded` = "false",
                       `aria-controls` = "howtoanswer4"),
          hidden(div(id = "howtoanswer4",
                     class = "howtoanswers",
                     HTML("P.I. surveys are not the only kind of plant survey. Another common type is a 'meander' survey, so called because the surveyor meanders in a zig-zag pattern around a lake rather than using pre-defined sampling locations like in a P.I. survey. Another key difference is that, often, other survey types are aimed only at finding one or a few specific taxa (especially invasives), whereas a P.I. survey will collect data on every plant taxon found. While both kinds of surveys are valuable, P.I. surveys are more comprehensive and less sensitive to a surveyor's assumptions. These two differences can also be used to tell the two survey types apart. For example, <a href='https://drive.google.com/file/d/1IE-k6VD-iPF0coQuXHLXN-Dhy1w1jVQD/view?usp=share_link', target = '_blank'>here is a sample report from a P.I. survey</a>. Notice, on page 11, the grid-like distribution of sampling locations. Now, <a href='https://drive.google.com/file/d/1wNZMSww-bd9efzZcbXZK95ZQYTL2sRYp/view?usp=share_link', target = '_blank'>here is a meander survey report</a>. Notice, on pages 2 and 3, how the sample locations follow a snaky path. Also, notice that the purpose of this meander survey was specifically to find invasive species, even though other species were sometimes noted."))),
          actionButton("howto5",
                       class = "howtomodalbuttons",
                       "Why Are P.I. Surveys Valuable?",
                       icon = icon("plus"),
                       `aria-expanded` = "false",
                       `aria-controls` = "howtoanswer5"),
          hidden(div(id = "howtoanswer5",
                     class = "howtoanswers",
                     HTML("P.I. surveys provide an unbiased and in-depth view of the health and diversity of waterbodies. They tell us how robust plant communities are, how invaded they are, where sensitive or ecologically important species are living, what kinds of conditions different taxa prefer, and how our floral communities are changing over time, among many other things. Simply put, P.I. survey data enable research that couldn't happen without them. If you want just a few examples of such research conducted by MAISRC teams, check out <a href='https://maisrc.umn.edu/native-restoration', target ='_blank'>this description of research on the post-management restoration of native vegetation</a> and <a href = 'https://maisrc.umn.edu/integrated-surveillance', target = '_blank'>this research on the early detection and monitoring of invasive aquatic plants using modeling empowered by P.I. survey data</a>."))),
          actionButton("howto6",
                       class = "howtomodalbuttons",
                       "Why is MAISRC Requesting and Collecting P.I. Surveys?",
                       icon = icon("plus"),
                       `aria-expanded` = "false",
                       `aria-controls` = "howtoanswer6"),
          hidden(div(id = "howtoanswer6",
                     class = "howtoanswers",
                     HTML("MAISRC wants to make P.I. survey data available for everyone. Every year, significant investments are made across Minnesota by state and local agencies, homeowners, lake managers, lake associations, restoration companies, and other stakeholders to conduct these surveys. However, while the <a href='https://www.dnr.state.mn.us/eco/pubs_aquatics/aquatic_reports.html', target ='_blank'>Minnesota DNR</a> keeps a database of surveys conducted by their teams and makes these available to the public, no one currently aggregates the data collected by non-state agencies and other sources, preventing these data from being explored and leveraged.<br><br>

While the methodologies used to collect these data are standardized across surveyors, the data management and storage practices vary by organization. P.I. Charter is designed to overcome the data science challenges raised by the disparity in data storage systems used--it can meaningfully combined even very different spreadsheets together into one spreadsheet. By submitting your P.I. survey data into P.I. Charter, you are contributing to a statewide database of these data accessible to all."))),
          actionButton("howto7",
                       class = "howtomodalbuttons",
                       "Why Does P.I. Charter Default to Using Scientific Names?",
                       icon = icon("plus"),
                       `aria-expanded` = "false",
                       `aria-controls` = "howtoanswer7"),
          hidden(div(id = "howtoanswer7",
                     class = "howtoanswers",
                     HTML("To make sure the data we are collecting is reliable, combinable, and universally interpretable, we refer to taxa by their scientific (Latin) names by default. Most importantly, scientific names are unambiguous, meaning each name refers to one and only one taxon--no two species can ever share the same scientific name. Meanwhile, common names change over time, regionally, and between dialects of the same language, and they differ across languages too. If you are unfamiliar with the scientific names of any taxa shown here, you may find it helpful to <a href = 'https://www.minnesotawildflowers.info/page/aquatics?pID=0', target = _blank>visit the Minnesota Wildflowers page on native aquatic species</a>, which provides both common and scientific names for many relevant taxa."))),
          actionButton("howto9",
             class = "howtomodalbuttons",
             "Contract Language for Getting P.I. Survey Data Submitted",
             icon = icon("plus"),
             `aria-expanded` = "false",
             `aria-controls` = "howtoanswer9"),
          hidden(div(id = "howtoanswer9",
           class = "howtoanswers",
           HTML("<p>If you're an organization that is contracted to collect P.I. survey data, or if you're an organization that contracts with companies or other entities to have P.I. surveys conducted on your lakes, <strong>we want your data!</strong> Below, we have prepared some generalized language you could include in contracts that indicates that survey data will be submitted to <b>P.I. Charter</b> as part of the contract unless one party opts out. Please feel free to use and adapt this language.</p><br<br>
                <h4>• For lake associations or government managers:</h4><br>
<blockquote>As a collaborating organization with the Minnesota Aquatic Invasive Species Research Center (MAISRC), we recognize the value of sharing plant survey data. We wish for our survey data to be shared with the research center and made available on their public database to inform management, monitor long-term trends, and support research. Our plant survey data can be submitted to MAISRC via <b>P.I. Charter</b> at z.umn.edu/PICharter.</blockquote>

<h4>• For contractors:</h4><br>
<blockquote>Our company collaborates with the Minnesota Aquatic Invasive Species Research Center (MAISRC). By default, all point intercept plant survey data will be shared with MAISRC researchers and made available on a public database to inform management, monitor long-term trends, and support research. The data will be submitted to MAISRC via <b>P.I. Charter</b>: <a href='z.umn.edu/PICharter' target='_blank'>z.umn.edu/PICharter</a>. If you do not wish for us to share your survey data, please notify us in writing.</blockquote>"))),
        )
      )
    )
  })
  
  #OBSERVER FOR SHOWING/HIDING THE FILTERS AND TOGGLES
  observeEvent(input$toggle_filters, {
    
    if(checkIfEven(input$toggle_filters)) {
      
      shinyjs::hide("hidden_filters", 
           anim = TRUE, 
           animType = "slide")
      
      updateActionButton(session,
                         "toggle_filters",
                         label = "  Show filters and toggles",
                         icon = icon("plus"))
      
      runjs(paste0("$('#toggle_filters').attr('aria-expanded', 'false');"))
      
    } else {
      
      shinyjs::show("hidden_filters", 
           anim = TRUE, 
           animType = "slide")
      
      updateActionButton(session,
                         "toggle_filters",
                         label = "  Hide filters and toggles",
                         icon = icon("minus"))
      
      runjs(paste0("$('#toggle_filters').attr('aria-expanded', 'true');"))
    }
  })
  
  #OBSERVER FOR SHOWING/HIDING THE DISCLAIMERS SECTION
  observeEvent(input$browse_disclaimers, {
    
    if(checkIfEven(input$browse_disclaimers)) {
      
      shinyjs::hide("browse_disclaimers_div", 
                    anim = TRUE, 
                    animType = "slide")
      
      updateActionButton(session,
                         "browse_disclaimers",
                         label = "  Show disclaimers",
                         icon = icon("plus"))
      
      runjs(paste0("$('#browse_disclaimers').attr('aria-expanded', 'false');"))
      
    } else {
      
      shinyjs::show("browse_disclaimers_div", 
                    anim = TRUE, 
                    animType = "slide")
      
      updateActionButton(session,
                         "browse_disclaimers",
                         label = "  Hide disclaimers",
                         icon = icon("minus"))
      
      runjs(paste0("$('#browse_disclaimers').attr('aria-expanded', 'true');"))
    }
  })
  
  #OBSERVERS FOR CONTROLLING THE SHOW/HIDE MECHANICS IN THE "HOW-TO" MODAL. ALL FOLLOW IDENTICAL PATTERN
observeEvent(input$howto0, {
    openCloseInfo(result = checkIfEven(input$howto0), "howtoanswer0","howto0", session)
})
observeEvent(input$howto1, {
    openCloseInfo(result = !checkIfEven(input$howto1), #START THIS ONE OPEN
                  "howtoanswer1","howto1", session)
})
observeEvent(input$howto2, {
  openCloseInfo(result = checkIfEven(input$howto2), "howtoanswer2","howto2", session)
})
observeEvent(input$howto3, {
  openCloseInfo(result = checkIfEven(input$howto3), "howtoanswer3","howto3", session)
})
observeEvent(input$howto4, {
  openCloseInfo(result = checkIfEven(input$howto4), "howtoanswer4","howto4", session)
})
observeEvent(input$howto5, {
  openCloseInfo(result = checkIfEven(input$howto5), "howtoanswer5","howto5", session)
})
observeEvent(input$howto6, {
  openCloseInfo(result = checkIfEven(input$howto6), "howtoanswer6","howto6", session)
})
observeEvent(input$howto7, {
  openCloseInfo(result = checkIfEven(input$howto7), "howtoanswer7","howto7", session)
})
observeEvent(input$howto8, {
  openCloseInfo(result = !checkIfEven(input$howto8), #START THIS ONE OPEN.
                "howtoanswer8","howto8", session)
})
observeEvent(input$howto9, {
  openCloseInfo(result = checkIfEven(input$howto9), "howtoanswer9","howto9", session)
})

#THIS OBSERVER WATCHES THE TABLE AND REPORTS TO SCREEN READER USERS ANY STATE CHANGES TO PAGE/LENGTH/RESULTS.
table_state = reactive({
  req(input$interactivemapdf_rows_all)
  req(input$interactivemapdf_state)
  
  list(
    n_matches = length(input$interactivemapdf_rows_all),
    start = input$interactivemapdf_state$start,
    length = input$interactivemapdf_state$length
  )
}) %>% debounce(750)

observe({
  state = table_state()
  page = floor(state$start / state$length) + 1
  total_pages = ceiling(state$n_matches / state$length)
  
  output$search_results_feedback <- renderUI({
    HTML(paste0(
      "<p>",
      "Showing page ", page, " of ", total_pages, ". ",
      state$n_matches, " result", ifelse(state$n_matches == 1, "", "s"), " total.",
      "</p>"
    ))
  })
})

}