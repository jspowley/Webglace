#' initialize UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_initialize_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
  )
}
    
#' initialize Server Parameters Contains small r inirstializations
#' @noRd
mod_initialize_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    
    r$remDr <- RSelenium::remoteDriver(
      remoteServerAddr = "selenium", # required for docker compose implementation
      # remoteServerAddr = "localhost",
      port = 4444L,
      browserName = "chrome"
    )
    
    init <- NA
    
    observeEvent(init, {
      try(r$remDr$close())
      r$remDr$open()
      r$remDr$navigate("https://duckduckgo.com")
    })
    
    init <- 1
    
  })
}
    
## To be copied in the UI
# mod_initialize_ui("initialize_1")
    
## To be copied in the server
# mod_initialize_server("initialize_1")
