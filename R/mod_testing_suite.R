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
              shiny::actionButton(ns(paste0("scrape_",n)), "Scrape")
            )
          })
        })
      })
      
      # reference to EIAtools and the accompanying stack overflow post for flexible inputs
      observe({ 
        lapply(names(r$selector_list), function(n) {
          id <- paste0("scrape_", n)
          observeEvent(input[[id]], {
            input_array$scrape <- list(id, input[[id]])
          })
        })
      })
      
      # Trigger for xpath vs css testing screen.
      observeEvent(input_array$scrape, {
        
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
        
        module_buffer$xpath <- xpath_nodes %>% as.character()
        module_buffer$css <- css_nodes %>% as.character()
        
      })
      
      observeEvent(list(module_buffer$xpath, module_buffer$css),{
        
        # For viewing xpath and css selector scrape data at the same time, to compare for consistency.
        # IFrames are very important here since it prevents header css and formatting from hijacking our app's formatting.
        
        shiny::req(module_buffer$xpath)
        shiny::req(module_buffer$css)
        
        # print(module_buffer$css)
        # html and head required since our main html structure is stripped from our scraping filters.
        html_css <- paste0(
          "<!DOCTYPE html><html><head><meta charset='UTF-8'></head><body>",
          module_buffer$css,
          "</body></html>",
          collapse = ""
        ) %>% 
          charToRaw() %>% 
          base64enc::base64encode() %>% 
          paste0("data:text/html;base64,", .) 
        # https://stackoverflow.com/questions/28766667/iframe-src-set-large-base64-data
        # https://github.com/MetaMask/metamask-mobile/issues/5441
        # Base64 as a convenient out for me not being able to figure out how iframe filepaths work in GOLEM
          
        # writeLines(html_css, "css.html")

        # print(module_buffer$xpath)
        
        html_xpath <- paste0(
          "<!DOCTYPE html><html><head><meta charset='UTF-8'></head><body>",
           module_buffer$xpath,
          "</body></html>",
          collapse = ""
        ) %>% 
          charToRaw() %>% 
          base64enc::base64encode() %>% 
          paste0("data:text/html;base64,", .)
        
        # writeLines(html_xpath, "xpath.html")
        
        output$testing_viewport <- 
          renderUI({
            bslib::layout_columns(
              bslib::card(
                bslib::card_header("CSS Scrape:"),
                tags$iframe(src = html_css, width = "100%", height = "1080px"),
                full_screen = TRUE
              ),
              bslib::card(
                bslib::card_header("XPath Scrape:"),
                tags$iframe(src = html_xpath, width = "100%", height = "1080px"),
                full_sreen = TRUE
              ),
              col_widths = c(6,6)
            )
          })
        
      })
      #facet_select <- sapply(facets, function(f_name){input[[paste0("f_",f_name)]]}, simplify = FALSE)
      
  })
}
    
## To be copied in the UI
# mod_testing_suite_ui("testing_suite_1")
    
## To be copied in the server
# mod_testing_suite_server("testing_suite_1")
