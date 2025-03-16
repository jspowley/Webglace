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
      title = "Classifiers 1",
      bslib::page_sidebar(
        sidebar = bslib::sidebar(
          shiny::actionButton(ns("add"), "Add Identifiers"),
          shiny::actionButton(ns("remove"), "Remove Identifiers"),
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
      
      r$remDr$navigate("https://example.com")
      
      js_test <- "return 2 + 2;"
      result <- r$remDr$executeScript(js_test, list())
      
      print(result)
      
      #css_selector <- "div"
      #js <- paste0("document.querySelectorAll('",css_selector,"')")
      #js <- 
      #  paste0(js,
      #    ".forEach(el => el.style.border = '2px solid red');"
      #  )
      #print(js)
      #js <- "document.body.style.backgroundColor = 'yellow';"
      #r$remDr$executeScript(js, args = list())
    })
  })
}
    
## To be copied in the UI
# mod_classification_1_ui("classification_1_1")
    
## To be copied in the server
# mod_classification_1_server("classification_1_1")

# References:
# https://www.w3schools.com/jsref/met_document_queryselectorall.asp
