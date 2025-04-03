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
      Sys.sleep(4)
      
      e <- list_button$xpath() %>% r$remDr$findElement(using = "xpath", value = .)
      r$remDr$executeScript("arguments[0].click();", list(e))
      
      Sys.sleep(4)
      filter_button$click(r$remDr)
      Sys.sleep(4)
      
      
# Getting Angular Scope
      
      # Trouble shooting angular has been nothing but brutal. I'm curious is tools such as pupeteer cover this in more robust fashion.
      
      # Click apartment type
      e <- r$remDr$findElement("xpath", apartment$xpath())
      r$remDr$executeScript("
  arguments[0].scrollIntoView({block: 'center'});
  arguments[0].click();
", list(e))
      Sys.sleep(2)
      
      # Click 2 bedroom
      e <- r$remDr$findElement("xpath", two_bed$xpath())
      r$remDr$executeScript("
  arguments[0].scrollIntoView({block: 'center'});
  arguments[0].click();
", list(e))
      Sys.sleep(2)
      
      # Click in-suite laundry
      e <- r$remDr$findElement("xpath", laundry$xpath())
      r$remDr$executeScript("
  arguments[0].scrollIntoView({block: 'center'});
  arguments[0].click();
", list(e))
      Sys.sleep(2)
      
      
      
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
