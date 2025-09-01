ui <- shiny::fluidPage( #Really, the whole UI is a navbarPage, but I wrap it in a fluidPage so I can stuff all the annoying "housekeeping elements here at the top, since navbarPage doesn't like receiving them.
  
  #FOR THE SKIP TO MAIN CONTENT LINK, WE NEED TO LET SHINY KNOW SO IT CAN CLOSE MODALS.
  tags$head(
    tags$script(HTML("
    window.addEventListener('hashchange', function() {
      if (window.location.hash === '#mainOverviewContent') {
        Shiny.setInputValue('skip_main_content', Math.random()); //Send new value every time to ensure it can be reused.
      }
    }, false);
  "))
  ),
  
  style = "margin-top: -45px;", 
  
  tags$html(lang = "en"), #SET THE PAGE LANGUAGE TO ENGLISH FOR SCREEN READERS.
  
  ## Insert a custom disconnect message so that folks know what the failure state looks like and can take appropriate actions
  disconnectMessage(text = " ",  #I DUNNO THAT THIS DOES ANYTHING IF WE'RE GONNA REPLACE IT WITH A JS SCRIPT ANYWAY.
                    refresh = "Refresh the page", 
                    width = "full", 
                    top = "center", 
                    size = 22, 
                    background = "#7a0019", 
                    colour = "white", 
                    refreshColour = "#ffb71e", 
                    css = "z-index: 100000 !important;"),

# Housekeeping items and head element stuff -------------------------------

#We use a nice waiter to show while the App preloads, with a custom spinner, text, and logo, plus styling and a fadeout.
  waiter::useWaiter(), 
  waiter::waiter_preloader(html = shiny::tagList(
    waiter::spin_loaders(6), shiny::br(), shiny::br(),
    shiny::HTML("Welcome to P.I. Charter, operated by the Minnesota Aquatic Invasive Species Research Center! Please wait while we chart your course..."), #Provides more text so that it doesn't need to go into the alt text.
    shiny::br(), shiny::br(),
    shiny::img(src = "preload.png", alt = "")), #THIS ALT CAN BE EMPTY--THE TEXT ABOVE CONVEYS ALL NECCESSARY INFO. 
    color = "#7a0019",
    fadeout = 500,
  ),

#Here, we load in our CSS stylesheet.
tags$head(tags$link(
  rel = "stylesheet", 
  type = "text/css",
  href = "PI_app.css"
),

#LOAD IN GOOGLE ANALYTICS SCRIPT
shiny::tags$head(
  shiny::tags$script(
    src = "https://www.googletagmanager.com/gtag/js?id=G-DZN3JZEM2V",
    async = ""
  ),
  shiny::tags$script(
    src = "js/googleanalytics.js"
  )
),

#SCRIPT FOR TAB PILL COLORATION ON SUBMISSIONS TAB
tags$head(
  tags$script(HTML("
  $(document).on('shiny:inputchanged', function(event) {
  
  //ON TAB CHANGE
  if (event.name === 'submission_tabset') {
  
    // RESET TAB PILL STYLES
    $('#submission_tabset>li').removeClass('active-previous');
    
    // FIND ACTIVE TAB, COLOR ALL TABS UP TO + INCLUDING IT
    var foundActive = false;
    $('#submission_tabset>li').each(function() {
      if ($(this).hasClass('active')) {
        foundActive = true;
        $(this).addClass('active-previous');
      } else if (!foundActive) {
        $(this).addClass('active-previous');
      }
    });
  }
});"))
),

#SCRIPT BLOCKING SCIENTIFIC NOTATION AND NEGATIVES FROM BEING ENTER FOR THESE TWO INPUTS
  tags$script(
    '
    $(document).on("keydown", "#start_row2", function (e) {
      if (e.key === "e" || e.key === "E" || e.key === "-") {
        e.preventDefault();
        return false;
      }
    });
    $(document).on("keydown", "#end_row2", function (e) {
      if (e.key === "e" || e.key === "E" || e.key === "-") {
        e.preventDefault();
        return false;
      }
    });
    
    '
  ),

#JS FUNCTION THAT FORCES THE DATE PICKER POPUP ON THE SUBMISSIONS TAB THAT COMES ALONG WITH SURVEY START DATE SELECTOR TO BE A BIT ABOVE THE SELECTOR SO IT DOESN'T COVER UP THE TYPING BOX.
tags$script(HTML("
$(document).ready(function(){
  // Function to adjust the datepicker position
  function adjustDatepickerPosition(input) {
    setTimeout(function(){
      var datepicker = $('body').find('.datepicker');
      var offset = $(input).offset();
      var dpHeight = datepicker.outerHeight();
      datepicker.css({
        top: (offset.top - dpHeight - 10) + 'px',
        left: offset.left + 'px'
      });
    }, 10);
  }

  // Bind focus and click events to the date input
  $('#survey_date2').on('focus click', function(){
    adjustDatepickerPosition(this);
  });

  // Also adjust position when the datepicker is shown
  $(document).on('shown.bs.datepicker', function(e){
    adjustDatepickerPosition($('#survey_date2'));
  });
});
")),

#JS FUNCTION THAT CAN RECEIVE A MESSAGE FROM SHINY SAYING, HEY, THE SESSION JUST ENDED. WHEN THAT HAPPENS, IT'LL GIVE THE SHINY DISCONNECT OVERLAY THE ARIA ROLE OF ALERTDIALOG, MAKE IT FOCUSABLE, AND MOVE FOCUS THERE. ALSO, ENSURE THE DISCONNECT MESSAGE REFRESH LINK ACTUALLY HAS TEXT IN IT. 
tags$script(HTML("
  $(document).on('shiny:disconnected', function(event) {
    var el = document.getElementById('ss-connect-dialog');
    if (el) {
      // Create an accessible alert
      var msgContainer = document.createElement('div');
      msgContainer.textContent = 'Hmm...something has gone wrong. Either you have been idle for too long and the app has timed out or an error has been triggered in the R code of the application. To try again, refresh the page. If, after doing so, you encounter this page again after taking the same actions as before, please file a bug report with Alex at bajcz003@umn.edu. We appreciate your cooperation!';
      msgContainer.setAttribute('role', 'alertdialog');
      msgContainer.setAttribute('tabindex', '-1');
      msgContainer.setAttribute('aria-label', 'Error message');
      msgContainer.style.outline = 'none';
      msgContainer.style.marginBottom = '1em';

      // Insert it at the beginning of the disconnect dialog
      el.insertBefore(msgContainer, el.firstChild);

      // Shift focus
      msgContainer.focus();
      
      // MAKE SURE THAT THE DISCONNECT LINK ALSO HAS TEXT IN IT:
      $(document).ready(function() {
      var reloadLink = document.getElementById('ss-reload-link');
      if (reloadLink) {
        reloadLink.innerText = 'Refresh the page';
       }
      });
    }
  });
"))

),

#THIS CUSTOM SCRIPT ALLOWS ME TO RESET THE HASH LOCATION IN THE URL SO THAT OUR SKIP TO MAIN CONTENT BUTTON CAN BE REUSED.
tags$script(HTML("
  Shiny.addCustomMessageHandler('clearSkipHash', function(message) {
    history.replaceState(null, '', window.location.pathname + window.location.search);
  });
")),

shinyjs::useShinyjs(), #ENABLE SHINYJS

#THIS IS A SHINY LIVE REGION THAT IS NOT VISIBLE TO ANYONE BUT IS SCREEN-READER-ENABLED. WHENEVER WE GENERATE A NOTIFICATION, WE CAN TRIGGER A MESSAGE TO GO INTO THIS REGION, WHERE IT'LL THEN BE READ ALOUD TO SCREEN-READER USERS. 
tags$div(id = "a11y-live-region", 
         `aria-live` = "assertive", 
         `aria-atomic` = "true", 
         class = "sr-only", 
         style = "position: absolute; width: 1px; height: 1px; margin: -1px; border: 0; padding: 0; overflow: hidden; clip: rect(0 0 0 0);"),

#HERE IS THE JAVASCRIPT CODE THAT WILL ALLOW THAT TO HAPPEN. IT'S TRIGGERED BY A SHINY CUSTOM MESSAGE, AND THEN INSERTS THE TEXT MESSAGE ARGUMENT INTO THE LIVE REGION.
tags$script(HTML("
  Shiny.addCustomMessageHandler('a11yLiveMessage', function(message) {
    var liveEl = document.getElementById('a11y-live-region');
    if (liveEl) {
      liveEl.textContent = message;
    }
  });
")),

# Core elements of the navbarPage -----------------------------------------

  shiny::navbarPage(
                 collapsible = T, #Yes, make it collapsible except I revoke this feature in the CSS--it seems cleaner to do it that way.
                 id = "theNavBar", 
                 lang =  "en",
                 title = "",  
                 header = 
                   tags$header( #MORE SEMANTICALLY APPROPRIATE DIV
                     class = "flexthis flexcol width100 justifycenter flexwrap",
                     h1(img(src = "PIlogocrop.png",
                         alt = "P.I. Charter.",#THIS ALT SHOULD JUST BE THE TEXT SIGHTED PERSONS WOULD SEE HERE.
                         style = "min-width: 320px;"),
                        class = "flexthis flexcol width100 justifycenter flexwrap"), #BY ADDING AN H1 HERE, SCREEN READERS WILL BE ABLE TO RECOGNIZE THIS AS A TOP-LEVEL HEADER.
                     fluidRow(
                       tags$nav( #SEMANTICALLY APPROPRIATE DIV
                         class = "flexthis flexwrap justifycenter",
                         style = "margin: 10px auto !important; align-items: center;",
                       actionButton(
                         "switch_browse",
                         h2(shiny::span(shiny::HTML('<span class="fa-solid fa-binoculars" aria-hidden="true"></span>'), #SPAN TAGS SHOULD BE USED FOR ICONS INSTEAD OF <i>
                           "Overview"
                         ), class = "navbuttons"), #BY WRAPPING IN H2s, SCREEN READERS WILL HOPEFULLY BE ABLE TO RECOGNIZE THESE AS 2ND-LEVEL HEADERS.
                         class = "navbuttons navbuttons_active" #STARTS WITH ACTIVE CLASS
                       ),
                       actionButton(
                         "switch_leaderboard",
                         h2(shiny::span(shiny::HTML('<span class="fa-solid fa-medal" aria-hidden="true"></span>'),
                           "Leaderboard"
                         ), class = "navbuttons"),
                         class = "navbuttons"
                       ),
                       div(id = "nav_last_two",
                         actionButton(
                         "switch_submissions",
                         h2(shiny::span(shiny::HTML('<span class="fa-solid fa-file-import" aria-hidden="true"></span>'),
                           "Submissions"
                         ), class = "navbuttons"),
                         class = "navbuttons"
                       ),
                       actionButton(
                         "switch_records",
                         h2(shiny::span(shiny::HTML('<span class="fa-solid fa-clipboard" aria-hidden="true"></span>'),
                           "Records"
                         ), class = "navbuttons"),
                         class = "navbuttons"
                       )
                     )
                     )
                    )
                   ), 
                
                 windowTitle = "P.I. Charter", #Our browser window blurb.
                 
                 #Insert a footer with links, contact info, funding credits, and current version number.
                 footer = tags$footer(shiny::HTML("<br><br>P.I. Charter was designed and is maintained by <address><a href='mailto:bajcz003@umn.edu'>Dr. Alex Bajcz (bajcz003@umn.edu)</a>, staff Quantitative Ecologist for <a href ='https://maisrc.umn.edu/ target = '_blank'>MAISRC</a></address>.<br><br>App last updated September 1 2025. Version 2.5.3. <a href = 'https://docs.google.com/document/d/1rQ6Kw9rzMbkKU-p9EGlcYTWbK25HGBgVWG9W2w-EA1g/edit?usp=sharing' target = '_blank'>View the accessibility statement (opens in a new tab)</a><br><br>
                                   Funding for this work was provided by the Minnesota Environment and Natural Resources Trust Fund (ENRTF) as recommended by the Minnesota Aquatic Invasive Species Research Center (MAISRC) and the Legislative-Citizen Commission on Minnesota Resources (LCCMR) and also the State of Minnesota.<br>"), 
                          shiny::img(src = "jointlogo.png",
                                     alt = "", #THIS ALT TEXT CAN BE EMPTY BECAUSE THE LOGOS ARE FUNCTIONALLY DECORATIVE--THE ESSENTIAL FUNDING INFO IS LISTED IN TEXT ABOVE THEM.
                                              style = "margin-top: 10px;
                                       margin-bottom: 10px;"),
                              id = "footerText",
                          style = "min-width: 320px;"),
                 
# The tabs

browseUI(),
leaderboardUI(), 
submissionsUI_V2(),
recordsUI(), 

)) #End UI
