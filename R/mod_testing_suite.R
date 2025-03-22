#' testing_suite UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_testing_suite_ui <- function(id) {
  
  ns <- NS(id)
  
    bslib::nav_panel(
      title = "Selector Flow Control"
    )
    
}
    
#' testing_suite Server Functions
#'
#' @noRd 
mod_testing_suite_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
      shiny::observeEvent(r$selector_list, {
        saveRDS(r$selector_list, "temp.rds")
      })
  })
}
    
## To be copied in the UI
# mod_testing_suite_ui("testing_suite_1")
    
## To be copied in the server
# mod_testing_suite_server("testing_suite_1")
