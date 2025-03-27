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
      )
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
    
      shiny::observeEvent(r$selector_list, {
        
        selectors <- list()
        
        for(i in 1:length(r$selector_list)){
          # Allows dynamic naming for lapply
          n <- names(r$selector_list)[i]
          selectors[i] <- list(r$selector_list[i])
          names(selectors)[i] <- n
        }
        
        saveRDS(selectors, "temp.rds")
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
      
      observeEvent(input_array$scrape, {
        print(str(input_array$scrape))
      })
      
      #facet_select <- sapply(facets, function(f_name){input[[paste0("f_",f_name)]]}, simplify = FALSE)
  })
}
    
## To be copied in the UI
# mod_testing_suite_ui("testing_suite_1")
    
## To be copied in the server
# mod_testing_suite_server("testing_suite_1")
