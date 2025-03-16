#' classification_1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_classification_1_ui <- function(id) {
  ns <- NS(id)
    bslib::nav_panel(
      title = "CSS Classification",
      bslib::page_sidebar(
        sidebar = bslib::sidebar(
          shiny::textInput(ns("css_select"), "CSS Selector"),
          shiny::actionButton(ns("add"), "Identify"),
          shiny::actionButton(ns("remove"), "Clear"),
        ),
        viewport_standalone()
      )
    )
}
    
#' classification_1 Server Functions
#'
#' @noRd 
mod_classification_1_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input$add,{
      
      css_selector <- input$css_select

      if(length(css_selector) > 0){
      
      if(!is.null(r$last_css)){
        # Removing previous selections if/when they exist
        js <- paste0("document.querySelectorAll('",r$last_css,"')")
        js <- 
          paste0(js,
                 ".forEach(el => el.style.border = '');"
          )
        r$remDr$executeScript(js, args = list(NULL))
      }
      
      js <- paste0("document.querySelectorAll('",css_selector,"')")
      js <- 
        paste0(js,
          ".forEach(el => el.style.border = '2px solid red');"
        )
      r$remDr$executeScript(js, args = list(NULL))
      
      r$last_css <- css_selector
      
      }
    })
    
    observeEvent(input$remove,{
      if(!is.null(r$last_css)){
        js <- paste0("document.querySelectorAll('",r$last_css,"')")
        js <- 
          paste0(js,
                 ".forEach(el => el.style.border = '');"
          )
        r$remDr$executeScript(js, args = list(NULL))
        r$last_css <- NULL
      }
    })
  })
}
    
## To be copied in the UI
# mod_classification_1_ui("classification_1_1")
    
## To be copied in the server
# mod_classification_1_server("classification_1_1")

# References:
# https://www.w3schools.com/jsref/met_document_queryselectorall.asp
