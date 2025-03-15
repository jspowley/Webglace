#' welcome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_welcome_ui <- function(id) {
  
  ns <- NS(id)
  
  bslib::nav_panel(
    title = "Welcome",
                   
    bslib::page_fluid(
      h1("Welcome!"),
      h2("About"),
      p("Webglace is an app designed for webscraping, allowing in app interaction with indentifiers, recursive functions, and data analysis modules. The goal in making this app is simple; to remove pain points in web scraping which arise from switching between the browser and development environment. The end goal for the application is to develop a modular classification system where any scrape in this environment can be downloaded and recreated within any R data science workflow, independent from it's 'In GUI' development. Not all intended features will be included on first ierations, however th multi application orchestration provides a strong starting point for the development of future tools to succeed."),
      h2("Using The App"),
      p("The app uses noVNC viewer and a standalone instance of selenium to allow session linked web browsing within the app. This simplifies orchestration and maintainability greatly but does come with some quirks. So for your quality of experience, here's a few tips:"),
      p("Scrolling in the viewport session when hovering over the desktop switches between sessions; If you ever find your browser window disappears unexpectedly, simply hover over the viewport desktop and scroll until you're back to workspace one. You can see which workspace you are in on the taskbar, at the bottom left. The easiest way to ensure a smooth us experience is to fullscreen the browser session, and to use only one tab at a time within the main browser.")
    )
  )
}
    
#' welcome Server Functions
#'
#' @noRd 
mod_welcome_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_welcome_ui("welcome_1")
    
## To be copied in the server
# mod_welcome_server("welcome_1")
