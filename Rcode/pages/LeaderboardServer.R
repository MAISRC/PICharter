leaderboardServer = function(input, output, session) {
  
  #Here's the surveyors leaderboard
output$leaderboard = DT::renderDT({
  surveyors.leaderboard.df
  }, 
    rownames = F,
    selection = 'none',
    caption = tags$caption("All surveyors in our database and their numbers of surveys."),
    options = base::list(paging = F,
                         info = FALSE,
                         language = list(
                           search = "Search surveyors: "
                         ),
               fillContainer = T),
  #TYPICAL DT TABLE SCREEN READER FIXES.
  callback = DT_A11Y_Callback
  )

#Here's the submitter's leaderboard
output$contrib_leaderboard = DT::renderDT({
  submitter_leaderboard
  },
    rownames = F,
    caption = tags$caption("All submitters in our database and their numbers of surveys submitted."),
    selection = 'none',
    options = base::list(fillContainer = T, 
                         info = FALSE,
                         paging = FALSE,
                         language = list(
                           search = "Search submitters: "
                         )),
  callback = DT_A11Y_Callback
  )

#GENERATE THE SPOKEN CONTENT FOR SCREEN READERS WHEN SEARCH RESULTS UPDATE
search_debounced2 = reactive({ input$leaderboard_search}) %>% debounce(750)

output$search_leaderboard_feedback = renderUI({
  req(search_debounced2())
  
  n_matches = length(isolate(input$leaderboard_rows_all)) #RESULTS OF SEARCH RETURNED
  
  if(search_debounced2() != "") { #MAKE SURE THEY'RE EVEN DOING A SEARCH.
    HTML(paste0("<p>", 
                n_matches, 
                " matching result", 
                ifelse(n_matches == 1, "", "s"), 
                " found.</p>"))
  } else {
    NULL  #DON'T PUT ANYTHING OTHERWISE.
  }
})

search_debounced3 = reactive({ input$contrib_leaderboard_search}) %>% debounce(750)
output$search_contrib_feedback = renderUI({
  req(search_debounced3())
  
  n_matches = length(isolate(input$contrib_leaderboard_rows_all)) #RESULTS OF SEARCH RETURNED
  if(search_debounced3() != "") { #MAKE SURE THEY'RE EVEN DOING A SEARCH.
    HTML(paste0("<p>", 
                n_matches, 
                " matching result", 
                ifelse(n_matches == 1, "", "s"), 
                " found.</p>"))
  } else {
    NULL  #DON'T PUT ANYTHING OTHERWISE.
  }
})

}