#' classification_1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_classification_1_ui <- function(id) {
  ns <- NS(id)
    bslib::nav_panel(
      title = "CSS Classification",
      bslib::page_sidebar(
        sidebar = bslib::sidebar(
          shiny::actionButton(ns("page_update"), "Pull HTML"),
          shiny::div(
            shiny::uiOutput(ns("tag_select")),
            shiny::uiOutput(ns("class_select")),
            shiny::div(
              shiny::uiOutput(ns("attr_name_select")),
              shiny::uiOutput(ns("attr_value_select")),
              style = select_div_style
            ),
            style = "margin-bottom: 0px; margin-top: 0;"
          ),
          shiny::textInput(ns("css_select"), "CSS Selector"),
          shiny::actionButton(ns("add"), "Identify"),
          shiny::actionButton(ns("remove"), "Clear"),
          shiny::actionButton(ns("test_js"), "Test JS"),
          width = 600
        ),
        viewport_standalone()
      )
    )
}
    
#' classification_1 Server Functions
#'
#' @noRd 
mod_classification_1_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Update html pull for forms
    shiny::observeEvent(input$page_update,{
      page_out <- r$remDr$getPageSource()
      page_out <- rvest::read_html(page_out[[1]])
      r$page_html <- page_out
    })
    
    # update tag form
    shiny::observeEvent(r$page_html, {
      print("updated tag form")
      tag_vec <- unique_tags(r$page_html)
      
      output$tag_select <- shiny::renderUI(
        shiny::div(
          style = select_div_style,
          shiny::selectInput(ns("tag_select"), "Tag", choices = tag_vec),
          shiny::actionButton(ns("tag_add"), "Add", style = select_add_style)
        )
        )
    })
    
    # update class form
    shiny::observeEvent(input$tag_select,{
      shiny::req(input$tag_select)
      if(length(input$tag_select) > 0){
        class_vec <- unique_classes(r$page_html, input$tag_select)
      }else{
        class_vec <- c("(No Class)")
      }
      
      output$class_select <- shiny::renderUI(
        shiny::div(
          style = select_div_style,
          shiny::selectizeInput(ns("class_select"), "Class", choices = append("(No Class)", class_vec), selected = "(No Class)"),
          shiny::actionButton(ns("class_add"), "Add", style = select_add_style)
        )
      )
    })
    
    # update attr name
    shiny::observeEvent(list(input$tag_select, input$class_select),{
      shiny::req(input$tag_select)
      shiny::req(input$class_select)
      if(input$class_select == "(No Class)"){
        attr <- r$page_html %>% attr_names(input$tag_select)
      }else{
        attr <- r$page_html %>% attr_names(input$tag_select, input$class_select)
      }
      print(attr)
      output$attr_name_select <- shiny::renderUI({
        shiny::selectizeInput(ns("attr_name_select"), "Attribute Name", choices = append("(No Attribute)", attr), selected = "(No Attribute)")
      })
    })
    
    # update attr applicable values
    shiny::observeEvent(list(input$tag_select, input$class_select, input$attr_name_select),{
      shiny::req(input$tag_select)
      shiny::req(input$class_select)
      if(input$class_select == "(No Class)"){
        attr <- r$page_html %>% attr_values(input$tag_select, input$attr_name_select)
      }else{
        attr <- r$page_html %>% attr_values(input$tag_select, class_in = input$class_select, input$attr_name_select)
      }
      print(attr)
      output$attr_value_select <- shiny::renderUI({
        shiny::selectizeInput(ns("attr_name_select"), "Attribute Name", choices = append("(No Attribute)", attr), selected = "(No Attribute)")
      })
    })
    
    # Adds the red boxin for the CSS selected.
    observeEvent(input$add,{
      
      css_selector <- input$css_select

      if(length(css_selector) > 0){
      
      if(!is.null(r$last_css)){
        # Removing previous selections if/when they exist
        js <- paste0("document.querySelectorAll('",r$last_css,"')")
        js <- 
          paste0(js,
                 ".forEach(el => el.style.border = '');"
          )
        r$remDr$executeScript(js, args = list(NULL))
      }
      
      js <- paste0("document.querySelectorAll('",css_selector,"')")
      js <- 
        paste0(js,
          ".forEach(el => el.style.border = '2px solid red');"
        )
      r$remDr$executeScript(js, args = list(NULL))
      
      r$last_css <- css_selector
      
      }
    })
    
    # Removes the red boxin for the css selected.
    observeEvent(input$remove,{
      if(!is.null(r$last_css)){
        js <- paste0("document.querySelectorAll('",r$last_css,"')")
        js <- 
          paste0(js,
                 ".forEach(el => el.style.border = '');"
          )
        r$remDr$executeScript(js, args = list(NULL))
        r$last_css <- NULL
      }
    })
    
    observeEvent(input$test_js,{
      r$remDr$executeScript("let elements = document.querySelectorAll('li.card_card___ZlNq'); elements.forEach(el => { let contains = el.querySelector('img') !== null; let within = el.closest('ul.cta-cards_cards__ApWvd') !== null;if(contains && within){el.style.border = '2px solid red'}});", args = list(NULL))
    })
  })
}

select_add_style <- "height: 37px; padding: 3px 10px; line-height: 1; margin-top: 16px;"
select_div_style <- "display: flex; align-items: center; gap: 5px; padding: 0;"
    
## To be copied in the UI
# mod_classification_1_ui("classification_1_1")
    
## To be copied in the server
# mod_classification_1_server("classification_1_1")

# References:
# https://www.w3schools.com/jsref/met_document_queryselectorall.asp
