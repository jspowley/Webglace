#' calgary_rentals UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_calgary_rentals_ui <- function(id) {
  
  ns <- NS(id)
  
  bslib::nav_panel(
    title = "Rent Scraper",
    
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
    
#' calgary_rentals Server Functions
#'
#' @noRd 
mod_calgary_rentals_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$display <- renderUI(viewport_standalone())
    
    observeEvent(input$view_browser, {
      output$display <- renderUI(viewport_standalone())  
    })
    
    observeEvent(input$run_script, {
      
      r$remDr$navigate("https://www.rentfaster.ca/ab/calgary/")
      Sys.sleep(2)
      list_button$click(r$remDr)
      Sys.ss
      filter_button$click(r$remDr)
      Sys.sleep(2)
      
      tryCatch({
        
        Sys.sleep(0.2)
        apartment$click(r$remDr, scroll_time = 1)
        Sys.sleep(2)
        two_bed$click(r$remDr, scroll_time = 1)
        Sys.sleep(2)
        laundry$click(r$remDr, scroll_time = 1)
        Sys.sleep(2)
        search_button$click(r$remDr, scroll_time = 1)
        
      },
      
      error = function(e){
        try({
          
          Sys.sleep(0.2)
          apartment2$click(r$remDr, scroll_time = 1)
          Sys.sleep(2)
          two_bed2$click(r$remDr, scroll_time = 1)
          Sys.sleep(2)
          laundry2$click(r$remDr, scroll_time = 1)
          Sys.sleep(2)
          search_button$click(r$remDr, scroll_time = 1)
          
        })
      })
      
      Sys.sleep(2)
      # card_scroll$scroll(r$remDr, 1000)
      
    })
    
  })
  
}

smooth_scroll <- function(remDr, px){
  js <- paste0("window.scrollBy({top: 1000, behavior: 'smooth'});")
  remDr$executeScript(js)
}
    
## To be copied in the UI
# mod_calgary_rentals_ui("calgary_rentals_1")
    
## To be copied in the server
# mod_calgary_rentals_server("calgary_rentals_1")
