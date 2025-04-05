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
    title = "Demo",
    
    bslib::page_sidebar(
      
      sidebar = bslib::sidebar(
    
        shiny::actionButton(ns("view_browser"), "View Browser"),
        shiny::numericInput(ns("iterations"), "Iterations (5-8 seconds per):", value = 5),
        shiny::checkboxInput(ns("enable_custom"), "Use Custom URL"),
        shiny::uiOutput(ns("custom_url_ui")),
        shiny::actionButton(ns("run_script"), "Run Script"),
        shiny::actionButton(ns("output_table"), "View Output Table"),
        shiny::downloadButton(ns("reddit_scrape"), "Download"),
        bslib::card(
          bslib::card_header("Description:"),
          p("A simple Reddit scraper."),
          p("We can use the scraped URL as a foreign to key link with more detailed information specific to each post. Other info such as upvotes and comment counts are hidden by a Shadow DOM requiring a more advanced kit."),
          p("To see it in action, click 'Run Script'. You can see data collection happen in browser."),
          p("When data collection is complete, you will see scrolling stop. You can click 'View Output Table' to view the scraped results.")
        ),
        width = 400
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
    
    # Slight break in molecularity, there's something strange about data patching on packages inside of Docker deployments
    # Found this by sshing into the container, for a later conversation with you, Phil

    print("FILE VISIBLE!")
    print(file.exists("/opt/my_dependencies/scrapedemo/data/post_time.rds"))
    
    print("POST EXISTS")
    print(getwd())
    print(list.files(getwd()))
    print(Sys.info()[["user"]])
    
    observeEvent(input$run_script, {
      
      try(post_in <- post)
      try(post_in_title <- post_title)
      try(post_in_time <- post_time[]
      
      try(post_in <- readRDS("/opt/my_dependencies/scrapedemo/data/post.rds"))
      try(post_in_title <- readRDS("/opt/my_dependencies/scrapedemo/data/post_title.rds"))
      try(post_in_ <- readRDS("/opt/my_dependencies/scrapedemo/data/post_time.rds"))
      
      if(input$enable_custom){
        try(r$remDr$navigate(input$custom_url))
      }else{
        r$remDr$navigate("https://www.reddit.com/r/worldnews/new/")
      }
      
      Sys.sleep(2)
      
      output_df <- NULL
      
      for(i in 1:input$iterations){
      
        print(i)
        
        for(x in 1:10){
          smooth_scroll(r$remDr, 500 + max(0,rnorm(1,0,20)))
          Sys.sleep(0.4 + max(0,rnorm(1,0,0.05)))
        }
        
        Sys.sleep(0.5)
      
        page <- r$remDr$getPageSource()
        page <- rvest::read_html(page[[1]])
        
        post_list <- page %>% post_in$scrape()
        
        for(p in post_list){
        
          title_vec <- p %>% post_in_title$text()
          url_vec <- p %>% post_in_title$href()
          time_vec <- p %>% post_in_time$scrape() %>% rvest::html_attr("datetime")
          
          if(is.null(output_df)){
            output_df <- data.frame(title = title_vec, url = url_vec, post_in_time = time_vec, scrape_time = Sys.time())
          }else{
            output_df <- dplyr::bind_rows(
              output_df,
              data.frame(title = title_vec, url = url_vec, post_in_time = time_vec, scrape_time = Sys.time())
            ) %>% 
              dplyr::distinct()
          }
          
        }
        
      }
      
      output_df <- output_df %>% 
        dplyr::mutate(title = stringr::str_remove_all(title, pattern = "\\n") %>% trimws())
      
      s$output_df <- output_df
      
      # Why are we unable to get votes and comment counts? JS on the page is using a ShadowDOM, which by default is not accessible by Selenium. 
      # JS toolkits such as puppeteer are necessary in this case.
      # It is also possible using execute script, however this falls outside of our current in scope toolkit, and will be pushed into future versioning.
        
    })
    
    observeEvent(input$enable_custom, {
      
      if(input$enable_custom){
        
        output$custom_url_ui <- renderUI({
          textInput(ns("custom_url"), "Custom Subreddit URL:", value = "https://www.reddit.com/r/worldnews/new/")
        })
        
      }else{
        
        output$custom_url_ui <- renderUI({
          p("")
        })
        
      }
      
    })
    
    
    observeEvent(input$output_table, {
      
      output$table_out <- DT::renderDT(s$output_df)
      
      output$display <- renderUI({
        DT::DTOutput(ns("table_out"))
      })
      
    })
    
    output$reddit_scrape <- shiny::downloadHandler(
      
      filename = "reddit.rds",
      
      content = function(file){
        saveRDS(s$output_df, file)
      },
      
      contentType = "application/octet-stream"
      
    )
    
  })
}
    
## To be copied in the UI
# mod_demo1_ui("demo1_1")
    
## To be copied in the server
# mod_demo1_server("demo1_1")
