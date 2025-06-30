shinyServer(function(input, output, session) {

# Navbar navigation -------------------------------------------------------
#WATCH BUTTONS, CHANGE TABS IF THE TAB PILL CLICKED IS NOT THAT OF THE CURRENT TAB.
observeEvent(input$switch_browse, {
   if(input$theNavBar != "1") {
    updateNavbarPage(session,
                     "theNavBar",
                     selected = "1")
   }
  #ADD AN ACTIVE CLASS TO THIS BUTTON AND REMOVE FROM ALL OTHERS.
  shinyjs::removeCssClass(selector = ".navbuttons", 
                          class = "navbuttons_active")
  shinyjs::addCssClass(id = "switch_browse", 
                    class = "navbuttons_active")

})
observeEvent(input$switch_leaderboard, {
    if(input$theNavBar != "2") {
      updateNavbarPage(session,
                       "theNavBar",
                       selected = "2")
    }
  shinyjs::removeCssClass(selector = ".navbuttons", 
                          class = "navbuttons_active")
  shinyjs::addCssClass(id = "switch_leaderboard", 
                       class = "navbuttons_active")
})
observeEvent(input$switch_submissions, {
  if(input$theNavBar != "3") {
    updateNavbarPage(session,
                     "theNavBar",
                     selected = "3")
  }
  shinyjs::removeCssClass(selector = ".navbuttons", 
                          class = "navbuttons_active")
  shinyjs::addCssClass(id = "switch_submissions", 
                       class = "navbuttons_active")
})
observeEvent(input$switch_records, {
  if(input$theNavBar != "4") {
    updateNavbarPage(session,
                     "theNavBar",
                     selected = "4")
  }
  shinyjs::removeCssClass(selector = ".navbuttons", 
                          class = "navbuttons_active")
  shinyjs::addCssClass(id = "switch_records", 
                       class = "navbuttons_active")
})

# The tabs ---------------------------------------------------------
browseServer(input, output, session)
leaderboardServer(input, output, session)
submissionsServer_V2(input, output, session)
recordsServer(input, output, session)


# Miscellaneous -----------------------------------------------------------
#IF THE USER USES THE SKIP TO MAIN CONTENT LINK, WE NEED TO CLOSE ANY MODALS OPEN
observe({
  req(input$skip_main_content)
  removeModal()
  session$sendCustomMessage("clearSkipHash", "") #CLEAR THE HASH TO MAKE IT REUSABLE.
})

}) #End Server Side
