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
    
    # Check if running in RStudio, setup specific to the testing versus deployment config
    # In future an in network ocker instance of RStudio server could be useful, 
    # though may raise security concerns due to embedded secret keys loaded directly to the image.
    if(shiny::isRunning() && rstudioapi::isAvailable()){
      network_name <- "localhost"
    }else{
      network_name <- "selenium"
    }
    
    r$remDr <- RSelenium::remoteDriver(
      # remoteServerAddr = "selenium", # required for docker compose implementation
      # remoteServerAddr = "localhost",
      remoteServerAddr = network_name,
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
