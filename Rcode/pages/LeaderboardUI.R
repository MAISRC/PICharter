#The structure of this tab is the simplest of the three by far--it's a glorified "acknowledgements" page. 
leaderboardUI = function() {
  shiny::tabPanel(value = 2, 
    shiny::div(shiny::span(shiny::HTML('<span class="fa-solid fa-medal" aria-hidden="true"></span>'), "Leaderboard")),
    shiny::fluidPage( #The tab is a single fluid container.
      style = "display: flex; flex-direction: column; padding-left: 0px; padding-right: 0px; min-width: 320px;",
      tags$main( #SEMANTICALLY APPROPRIATE DIV
      
      ## Leaderboards tab, top elements (explainers) ----------------------------------------
      #It starts with a single brief explainer header/paragraph like seen on the browse tab. 
      shiny::div(id = "leaderboardPara", 
                 class = "centertext",
        p("This tab is where we say thanks to everyone who conducted the surveys in our database and also to everyone who graciously submitted those surveys to us. We thank you!")),
      
      ## Leaderboards tab, middle elements (table headers and leaderboard tables) ----------------------------------------
      shiny::fluidRow( #These are all one row, divided into two columns, one per leaderboard.
        ### Leaderboards tab, middle elements, left column (surveyors) ----------------------------------------
        shiny::column(width=6,  
                      tags$section( #SEMANTICALLY APPROPRIATE DIV
               #Thank those who performed the actual fieldwork.
               div(HTML("<h3 tabIndex = '0'>Surveyors Leaderboard</h3>"),
                   class = "leaderboardSubParas"), shiny::br(),
               #THIS DIV IS JUST HERE TO READ TO SCREEN READERS THE SEARCH RESULTS.
               tags$div(
                 `aria-live` = "polite",
                 `aria-atomic` = "true",
                 class = 'sr-only',
                 role = "status",
                 id = "results_live_region",
                 uiOutput("search_leaderboard_feedback")
               ),
               #The surveyors leaderboard table sorted by #s of surveys participated in.
               shiny::div(DT::dataTableOutput("leaderboard", width = "100%", height = "375px"),
                   class = "standardTable leaderboards")
                )
               ),
        
        ### Leaderboards tab, middle elements, right column (submitting orgs) ----------------------------------------
        shiny::column(width = 6, #Same deal except those who submitted the data.
                      tags$section( #SEMANTICALLY APPROPRIATE DIV
                      shiny::div(shiny::HTML("<h3 tabIndex = '0'>Submitters Leaderboard</h3>"), 
                   class = "leaderboardSubParas"), shiny::br(), 
               #THIS DIV IS JUST HERE TO READ TO SCREEN READERS THE SEARCH RESULTS.
               tags$div(
                 `aria-live` = "polite",
                 `aria-atomic` = "true",
                 class = 'sr-only',
                 role = "status",
                 id = "results_live_region",
                 uiOutput("search_contrib_feedback")
               ),
               #The submitters leaderboard table sorted by #s of surveys submitted.
               shiny::div(DT::dataTableOutput("contrib_leaderboard", width = "100%", height = "375px"),
                   class = "standardTable leaderboards"))))
    )
   )
  )
}