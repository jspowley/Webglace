#' viewport UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_browser_ui <- function(id) {
  
  ns <- NS(id)
  
  bslib::nav_panel(title = "Browser",
                   
    bslib::page_sidebar(
      sidebar = bslib::sidebar(
        
        bslib::card(
          bslib::card_header("Setup"),
          actionButton(ns("start_browser"), "Start Browser")
        ),
        
        width = 600
      ),
      viewport_standalone()
    )
  )
}
    
#' viewport Server Functions
#'
#' @noRd 
mod_browser_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
    shiny::observeEvent(input$start_browser,{
      r$remDr$open()
    })
    
  })
}

viewport_standalone <- function(){
  # No input elements required, and therefore, no namespace necessary. Standalone function which can be embedded within other modules when necessary.
  tagList(
    tags$iframe(src = "http://localhost:6900/vnc.html?autoconnect=true&password=secret", width = "100%", height = "1080px")
  )
}
    
## To be copied in the UI
# mod_viewport_ui("viewport_1")
    
## To be copied in the server
# mod_viewport_server("viewport_1")
