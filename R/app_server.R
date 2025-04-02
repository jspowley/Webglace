#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  r <- shiny::reactiveValues()
  
  mod_initialize_server("initialize", r)
  mod_browser_server("browser_1", r)
  mod_classification_1_server("class_1", r)
  mod_testing_suite_server("testing_1", r)
  mod_guitar_server("guitar", r)
  
  # bslib::bs_themer()
  
}
