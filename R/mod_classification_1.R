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
          shiny::actionButton(ns("test_js"), "Test JS")
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
    
    # Adds the red boxin for the CSS selected.
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
    
    # Removes the red boxin for the css selected.
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
    
    observeEvent(input$test_js,{
      r$remDr$executeScript("let elements = document.querySelectorAll('li.card_card___ZlNq'); elements.forEach(el => { let contains = el.querySelector('img') !== null; let within = el.closest('ul.cta-cards_cards__ApWvd') !== null;if(contains && within){el.style.border = '2px solid red'}});", args = list(NULL))
    })
  })
}
    
## To be copied in the UI
# mod_classification_1_ui("classification_1_1")
    
## To be copied in the server
# mod_classification_1_server("classification_1_1")

# References:
# https://www.w3schools.com/jsref/met_document_queryselectorall.asp
