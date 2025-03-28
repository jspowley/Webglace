#' viewport UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_browser_ui <- function(id) {
  
  ns <- NS(id)
    
  bslib::nav_panel(title = "Browser",
                   
    bslib::page_sidebar(
      sidebar = bslib::sidebar(
        
        # Setup
        bslib::card(
          bslib::card_header("Setup and Browser"),
          shiny::actionButton(ns("start_browser"), "Start Browser")
        ),
        
        # Browse by URL
        bslib::card(
          bslib::card_header("Site URL"),
          shiny::textInput(ns("url"), "", value = "https://duckduckgo.com"),
          shiny::actionButton(ns("go"), "Go!")
        ),
        
        # For pulling data downstream into other modules and downloading raw html
        bslib::card(
          bslib::card_header("Pull"),
          shiny::actionButton(ns("pull_html"), "Pull HTML"),
          shiny::downloadButton(ns("download_html"), "Download HTML")
        ),
        
        # Preview panel
        bslib::card(
          bslib::card_header("HTML Preview"),
          shiny::selectizeInput(ns("preview_mode"), "Preview Mode", choices = c("Static Preview", "Raw", "No Preview"), selected = "No Preview"),
          shiny::hr(),
          shiny::verbatimTextOutput(ns("html_raw")),
          shiny::uiOutput(ns("html_preview")),
          full_screen = TRUE
        ),
        
        width = 600
      ),
      viewport_standalone()
    )
  )
}
    
#' viewport Server Functions
#'
#' @noRd 
mod_browser_server <- function(id, r){
  
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
 
    # opening a new browser instance
    shiny::observeEvent(input$start_browser,{
      try(r$remDr$close())
      r$remDr$open()
      onStop(function(){
        # Attempt to make the browser close when the session ends. we've tried passing in r explicitly, accessing implicitly. Doesn't work.
        # That being said, since we are in a docker context where opening/closing doesn't actually re-init, we're just observing on a localhost,
        # this raises no issues in terms of user interaction.
        closing_trigger <- NA
        observeEvent(closing_trigger,{r$remDr$close()})
        closing_trigger <- 1
      })
    })
    
    # Navigating to a URL
    shiny::observeEvent(input$go, {
      
      url_in <- input$url
      
      # http is required at the request level
      if(!stringr::str_detect(url_in, pattern = "http")){
        url_in <- paste0("http://", url_in)
      }
      
      r$remDr$navigate(
        url_in
      )
      
    })
    
    # Connecting page html to other app sub-modules
    shiny::observeEvent(input$pull_html,{
      
      page_out <- r$remDr$getPageSource()
      page_out <- rvest::read_html(page_out[[1]])
      r$page_html <- page_out
      
      print(r$page_html)
      saveRDS(as.character(page_out), "test_page.rds")
      
    })
    
    # For downloading static html. This can help troubleshoot what RSelenium can see.
    output$download_html <- shiny::downloadHandler(
      "html_out.html",
      
      content = function(file) {
        page_out <- r$remDr$getPageSource()
        page_out <- rvest::read_html(page_out[[1]])
        xml2::write_html(page_out, file)
      },
      
      contentType = "text/html"
      
    )
    
    # Displaying the pulled page currently queued for downstream use
    observeEvent(list(r$page_html, input$preview_mode), {
      
      shiny::req(r$page_html)
      shiny::req(input$preview_mode)
      
      # Provides a raw HTML view of the page
      if(input$preview_mode == "Raw"){
        
        output$html_raw <- shiny::renderText({
          
          path <- tempfile()
          xml2::write_html(r$page_html, path)
          as.character(xml2::read_html(path))
          
        })
        
        output$html_preview <- shiny::renderUI({""})
        
        
      }
      
      # Provides a visual view of the page
      if(input$preview_mode == "Static Preview"){
        
        output$html_raw <- shiny::renderText({""})
        
        output$html_preview <- shiny::renderUI({
          
          # Duplicated from the testing suite, this is where I originally noticed the header hijack bug.
          html_64 <- paste0(
            "<!DOCTYPE html><html><head><meta charset='UTF-8'></head><body>",
            r$page_html,
            "</body></html>",
            collapse = ""
          ) %>% 
            charToRaw() %>% 
            base64enc::base64encode() %>% 
            paste0("data:text/html;base64,", .)
          
          tags$iframe(src = html_64, width = "100%", height = "1080px")
          
        })
        
      }
      
      if(input$preview_mode == "No Preview"){
        output$html_raw <- shiny::renderText({""})
        output$html_preview <- shiny::renderUI({""})
      }
        
      })
    
  })
}

viewport_standalone <- function(){
  # No input elements required, and therefore, no namespace necessary. Standalone function which can be embedded within other modules when necessary.
  tagList(
    tags$iframe(src = "http://localhost:6900/vnc.html?autoconnect=true&password=secret", width = "100%", height = "1080px")
  )
}
    
## To be copied in the UI
# mod_viewport_ui("viewport_1")
    
## To be copied in the server
# mod_viewport_server("viewport_1")
