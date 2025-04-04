#' demo1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_demo1_ui <- function(id) {
  ns <- NS(id)
  
  bslib::nav_panel(
    title = "Demo 1",
    
    bslib::page_sidebar(
      
      sidebar = bslib::sidebar(
        p("A simple Reddit scraper. We can get anchor URL for follow up scraping and headlines. Other info such as upvotes and comment counts are hidden by a Shadow DOM requiring a more advanced kit."),
        actionButton(ns("view_browser"), "View Browser"),
        actionButton(ns("run_script"), "Run Script"),
        actionButton(ns("output_table"), "View Output Table"),
        actionButton(ns("output_cloud"), "View Output Word Cloud")
      ),
      
      uiOutput(ns("display"))
    )
  )
}
    
#' demo1 Server Functions
#'
#' @noRd 
mod_demo1_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    s <- reactiveValues()
 
    output$display <- renderUI(viewport_standalone())
    
    observeEvent(input$view_browser, {
      output$display <- renderUI(viewport_standalone())  
    })
    
    observeEvent(input$run_script, {
      
      r$remDr$navigate("https://www.reddit.com/r/worldnews/")
      Sys.sleep(2)
      
      output_df <- NULL
      
      for(i in 1:1){
      
        print(i)
        
        for(x in 1:10){
          smooth_scroll(r$remDr, 500 + max(0,rnorm(1,0,20)))
          Sys.sleep(0.4 + max(0,rnorm(1,0,0.05)))
        }
      
        page <- r$remDr$getPageSource()
        page <- rvest::read_html(page[[1]])
        
        post_list <- page %>% post$scrape()
      
        title_vec <- post_list %>% post_title$text()
        url_vec <- post_list %>% post_title$href()
      
        if(is.null(output_df)){
          output_df <- data.frame(title = title_vec, url = url_vec)
        }else{
          output_df <- dplyr::bind_rows(
            output_df,
            data.frame(title = title_vec, url = url_vec)
          ) %>% 
            dplyr::distinct()
        }
        
      }
      
      output_df <- output_df %>% 
        dplyr::mutate(title = stringr::str_remove_all(title, pattern = "\\n") %>% trimws())
      
      s$output_df <- output_df
      
      # Why are we unable to get votes and comment counts? JS on the page is using a ShadowDOM, which by default is not accessible by Selenium. 
      # JS toolkits such as puppeteer are necessary in this case.
      # It is also possible using execute script, however this falls outside of our current in scope toolkit, and will be pushed into future versioning.
        
    })
    
  })
}
    
## To be copied in the UI
# mod_demo1_ui("demo1_1")
    
## To be copied in the server
# mod_demo1_server("demo1_1")
