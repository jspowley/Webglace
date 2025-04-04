#' lessons UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_lessons_ui <- function(id) {
  ns <- NS(id)
  bslib::nav_panel(
    title = "Learnings",
    uiOutput(ns("learnings"))
  )
}
    
#' lessons Server Functions
#'
#' @noRd 
mod_lessons_server <- function(id){
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    
    quarto::quarto_render(
      input = "www/learnings.qmd"
    )
    
    output$learnings <- renderUI({
      tags$iframe(
        src = "learnings.html",
        width = "100%",
        height = "600px"
      )
    })
 
  })
}
    
## To be copied in the UI
# mod_lessons_ui("lessons_1")
    
## To be copied in the server
# mod_lessons_server("lessons_1")
