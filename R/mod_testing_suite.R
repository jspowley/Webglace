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
    
    title = "Testing and Export",
    
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 600, 
        bslib::card(
          shiny::downloadButton(ns("download_all"), "Download All", icon = shiny::icon("download"))
        ),
        uiOutput(ns("selector_cards"))
      ),
      uiOutput(ns("testing_viewport"))
    )
  )
    
}
    
#' testing_suite Server Functions
#'
#' @noRd 
mod_testing_suite_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
      
      # Initiating dynamic input lists
      input_array <- reactiveValues()
      module_buffer <- reactiveValues()
    
      shiny::observeEvent(r$selector_list, {
        
        selectors <- list()
        
        for(i in 1:length(r$selector_list)){
          # Allows dynamic naming for lapply
          n <- names(r$selector_list)[i]
          selectors[i] <- list(r$selector_list[i])
          names(selectors)[i] <- n
        }
        
        print(str(selectors))
        
        output$selector_cards <- renderUI({
          lapply(selectors, function(s) { #lapply handles the UI context better, based on a few stack overflow threads. Not my typical workflow but manages niche cases like this.
            n <- names(s)[1]
            print(n)
            bslib::card(
              bslib::card_header(paste0("Name: ",n)),
              shiny::div(style = select_div_style_2,
                shiny::actionButton(ns(paste0("scrape_",n)), "Scrape", style = select_add_style_2),
                shiny::actionButton(ns(paste0("links_",n)), "Pull Links", style = select_add_style_2),
                shiny::actionButton(ns(paste0("text_",n)), "Pull Text", style = select_add_style_2),
                shiny::actionButton(ns(paste0("browser_",n)), "View Browser", style = select_add_style_2),
                shiny::actionButton(ns(paste0("click_",n)), "Click Element", style = select_add_style_2)
              ),
              shiny::fluidRow(
                shiny::div(
                  style = "padding-right: 30px;",
                  bslib::layout_columns(
                    style = "gap: 5px; padding: 0;",
                    shiny::actionButton(
                      ns(paste0("properties_",n)), 
                      "View Properties", 
                      sytle = select_add_style_2),
                    shiny::downloadButton(
                      ns(paste0("download_",n)), 
                      label = NULL, 
                      icon = shiny::icon("download"),
                      download_attr = list(download = "my_selector.rds")),
                    col_widths = c(10,2)
                  )
                )
              )
            )
          })
        })
      })
      
      # reference to EIAtools and the accompanying stack overflow post for flexible inputs
      observe({ 
        
        print("RESETTING FLEXIBLE INPUT ARRAY")
        
        lapply(names(r$selector_list), function(n) {
          id <- paste0("scrape_", n)
          observeEvent(input[[id]], {
            input_array$scrape <- list(id, input[[id]])
          })
        })
      
      # Link updates
        lapply(names(r$selector_list), function(n) {
          id <- paste0("links_", n)
          observeEvent(input[[id]], {
            input_array$links <- list(id, input[[id]])
          })
        })
      
        # Text updates
        lapply(names(r$selector_list), function(n) {
          id <- paste0("text_", n)
          observeEvent(input[[id]], {
            input_array$text <- list(id, input[[id]])
          })
        })
        
        # Switch to browser view for live interaction testing
        lapply(names(r$selector_list), function(n) {
          id <- paste0("browser_", n)
          observeEvent(input[[id]], {
            input_array$browser <- list(id, input[[id]])
          })
        })
        
        # Clicking an xpath element
        lapply(names(r$selector_list), function(n) {
          id <- paste0("click_", n)
          observeEvent(input[[id]], {
            input_array$click <- list(id, input[[id]])
          })
        })
        
        # Download Widgets, by Selector
        lapply(names(r$selector_list), function(n) {
          id <- paste0("download_", n)
          n <- stringr::str_remove(id, pattern = "download_") %>% paste0(".rds")
          print(n)
          output[[id]] <- shiny::downloadHandler(
            
            filename = function() n,
            content = function(file){
      
              saveRDS(r$selector_list[[n]], file)
              
            },
            contentType = "application/octet-stream"
          )
        })
        
        # For initalizing the prpoerties and documentation view
        lapply(names(r$selector_list), function(n) {
          id <- paste0("properties_", n)
          observeEvent(input[[id]], {
            input_array$properties <- list(id, input[[id]])
          })
        })
        
      })
      
      observeEvent(input_array$click,{
        
        selector_name <- stringr::str_remove(input_array$click[1], pattern = "click_")
        selector_in <-  r$selector_list[[selector_name]]
        
        try(selector_in$click(r$remDr))
        
      })
      
      # Triggers the text for text pull
      observeEvent(input_array$text, {
        
        print("TEXT PRINT OBSERVED")
        
        page_out <- r$remDr$getPageSource()
        page_out <- rvest::read_html(page_out[[1]])
        selector_name <- stringr::str_remove(input_array$text[1], pattern = "text_")
        selector_in <-  r$selector_list[[selector_name]]
        
        text_within <- selector_in$text(page_out)
        
        output$testing_viewport <- renderUI({
          bslib::card(
            bslib::card_header("Text Content"),
            HTML(paste(text_within, collapse = "<br>"))
          )
        })
        
      })
      
      # Triggers the href attribute pull test.
      observeEvent(input_array$links, {
        
        print("HREF PRINT OBSERVED")
        
        page_out <- r$remDr$getPageSource()
        page_out <- rvest::read_html(page_out[[1]])
        selector_name <- stringr::str_remove(input_array$links[1], pattern = "links_")
        selector_in <-  r$selector_list[[selector_name]]
        
        href_within <- selector_in$href(page_out)
        
        output$testing_viewport <- renderUI({
          bslib::card(
            bslib::card_header("Hyperlinks"),
            HTML(paste(href_within, collapse = "<br>"))
          )
        })
        
        
        
      })
      
      # Trigger for xpath vs css testing screen.
      observeEvent(input_array$scrape, {
        
        print("SCRAPE OBSERVED")
        
        page_out <- r$remDr$getPageSource()
        page_out <- rvest::read_html(page_out[[1]])
        
        selector_name <- stringr::str_remove(input_array$scrape[1], pattern = "scrape_")
        selector_in <-  r$selector_list[[selector_name]]
        
        xpath_in <- selector_in$xpath()
        
        print(xpath_in)
        
        xpath_nodes <- 
          page_out %>% 
          rvest::html_elements(xpath = xpath_in)
        
        css_nodes <- selector_in$scrape(page_out)
        
        xpath <- xpath_nodes %>% as.character()
        css <- css_nodes %>% as.character()
        
        html_css <- paste0(
          "<!DOCTYPE html><html><head><meta charset='UTF-8'></head><body>",
          css,
          "</body></html>",
          collapse = ""
        ) %>% 
          charToRaw() %>% 
          base64enc::base64encode() %>% 
          paste0("data:text/html;base64,", .) 
        # https://stackoverflow.com/questions/28766667/iframe-src-set-large-base64-data
        # https://github.com/MetaMask/metamask-mobile/issues/5441
        # Base64 as a convenient out for me not being able to figure out how iframe filepaths work in GOLEM
        
        html_xpath <- paste0(
          "<!DOCTYPE html><html><head><meta charset='UTF-8'></head><body>",
          xpath,
          "</body></html>",
          collapse = ""
        ) %>% 
          charToRaw() %>% 
          base64enc::base64encode() %>% 
          paste0("data:text/html;base64,", .)
        
        output$testing_viewport <- 
          renderUI({
            bslib::layout_columns(
              bslib::card(
                bslib::card_header("CSS Scrape:"),
                tags$iframe(src = html_css, width = "100%", height = "650px"),
                full_screen = TRUE
              ),
              bslib::card(
                bslib::card_header("XPath Scrape:"),
                tags$iframe(src = html_xpath, width = "100%", height = "650px"),
                full_sreen = TRUE
              ),
              col_widths = c(6,6)
            )
          })
        
      })
      
      observeEvent(input_array$browser,{
        output$testing_viewport <- renderUI({
          viewport_standalone()
        })
      })
      
      output$download_all <- shiny::downloadHandler(
        
        filename = function() "selector_list.zip",
        
        content = function(file) {
          
          dir <- file.path(tempdir(), "selector_list_files")
          dir.create(dir)
          
          n_list <- names(r$selector_list)
          paths <- file.path(dir, paste0(n_list, ".rds"))
          
          for (i in 1:length(n_list)) {
            saveRDS(r$selector_list[[n_list[i]]], paths[i])
          }
          
          zip(zipfile = file, files = paths, flags = "-j")
        
        },
        
        contentType = "application/zip"
        
      )
      
      # For displaying the structure and implementation documentation of a selector
      observeEvent(input_array$properties,{
        
        selector_name <- stringr::str_remove(input_array$properties[1], pattern = "properties_")
        selector_in <-  r$selector_list[[selector_name]]
        
        output$code1 <- renderText({
 
        paste0(         
"
# RSelenium to be previously intiated and assigned to variable remDr
selector <- readRDS('",
selector_name, ".rds')
          
html <- remDr$getPageSource()
html <- rvest::read_html(html[[1]])
          
# Scrapes a webpage using the CSS selectors recursively
selector$scrape(html)
          
# Since outputs serve as valid inputs to the scrape function, mutliple selectors can be chained for advanced filtering:
selector$scrape(html) %>% selector2$scrape()
          
# Getting the curent xpath for custom JS / Selenium:
selector$xpath()
          
# When text matching is desired in the xpath, use the following. Exact can be TRUE/FALSE to modify whether a substring or exact match is desired.
selector$xpath_text('text to match', exact = FALSE)

# Pulling href/urls off the page. This is particular to the current level element, and won't apply to child elements lower down. 
# na.rm removes NA values for nodes not containing an href attribute.
selector$href(html, rm.na = TRUE)

# Text pulls any text nested within nodes
selector$text(html)

# For clicking dynamic elements. For more advanced interaction, xpath is designed to be able to target elements easily within a JS framework, and is recommended.
# Text matching is optional, but helpful for targetting buttons very in singular fashion.
selector$click(remDr, text = NULL, exact_text = FALSE)
"
)
          
        })
        
        output$testing_viewport <- renderUI({
          
          bslib::layout_columns(
            
            bslib::card(
              bslib::card_header(paste0("Name: ", selector_name)),
              p(paste0("CSS Parent: ", selector_in$css_within)),
              p(paste0("CSS Main: ", selector_in$css_self)),
              p(paste0("CSS Child: ", selector_in$css_contains)),
              p(paste0("XPath: "), selector_in$xpath())
            ),
            
            bslib::card(
              bslib::card_header(paste0("Implementation")),
              verbatimTextOutput(ns("code1"))
            ),
            
            col_widths = c(12,12)
            
          )
          
        })
        
      })
        
  })
}

select_add_style_2 <- "height: 37px; padding: 3px 10px; line-height: 1; margin-top: 16px;"
select_div_style_2 <- "display: flex; align-items: center; gap: 5px; padding: 0;"
css_diag_style_2 <- 'style="border: 2px solid red; padding: 10px; word-wrap: break-word;"'
    
## To be copied in the UI
# mod_testing_suite_ui("testing_suite_1")
    
## To be copied in the server
# mod_testing_suite_server("testing_suite_1")
