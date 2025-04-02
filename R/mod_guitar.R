#' guitar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_guitar_ui <- function(id) {
  ns <- NS(id)
  
  bslib::nav_panel(
    title = "Harmonic Progressions",
    
    bslib::page_sidebar(
      
      sidebar = bslib::sidebar(
        p("Simple script for pulling rents for 2 bedroom apartments in Calgary."),
        actionButton(ns("view_browser"), "View Browser"),
        actionButton(ns("run_script"), "Run Script")
      ),
      
      uiOutput(ns("display"))
    )
  )
}
    
#' guitar Server Functions
#'
#' @noRd 
mod_guitar_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    output$display <- renderUI(viewport_standalone())
    
    observeEvent(input$view_browser, {
      output$display <- renderUI(viewport_standalone())  
    })
    
    observeEvent(input$run_script, {
      
      r$remDr$navigate("https://ultimate-guitar.com/fresh")
      
    })
  })
}
    
## To be copied in the UI
# mod_guitar_ui("guitar_1")
    
## To be copied in the server
# mod_guitar_server("guitar_1")
