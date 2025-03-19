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
            
            shiny::div(style = select_div_style,
              shiny::uiOutput(ns("tag_select")),
              shiny::actionButton(ns("tag_add"), "Add", style = select_add_style),
              shiny::actionButton(ns("tag_remove"), "Remove", style = select_add_style)
            ),
            
            shiny::div(style = select_div_style,
              shiny::uiOutput(ns("class_select")),
              shiny::actionButton(ns("class_add"), "Add", style = select_add_style),
              shiny::actionButton(ns("class_remove"), "Remove", style = select_add_style)
            ),
            
            shiny::div(
              shiny::uiOutput(ns("attr_name_select")),
              shiny::uiOutput(ns("attr_value_select")),
              shiny::actionButton(ns("attr_add"),"Add", style = select_add_style),
              shiny::actionButton(ns("attr_remove"),"Remove", style = select_add_style),
              style = select_div_style
            ),
            
            shiny::div(
              shiny::uiOutput(ns("css_select_ui")),
              shiny::actionButton(ns("remove"), "Clear", style = select_add_style),
              style = select_div_style
            ),
            
            bslib::layout_columns(
              shiny::actionButton(ns("to_parent"), "Add to Parent"),
              shiny::actionButton(ns("to_main"), "Add to Main"),
              shiny::actionButton(ns("to_child"), "Add to Child"),
              col_widths = c(4,4,4)
            ),
            
            style = "margin-bottom: 0px; margin-top: 0;"
          ),
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

    # Initialize form:
    
    output$tag_select <- renderUI(p("To begin, Load HTML"))
    output$class_select <- renderUI(p("To begin, Load HTML"))
    output$attr_name_select <- renderUI(p("To begin, Load HTML"))
    output$attr_value_select <- renderUI(p())
    
    css <- shiny::reactiveValues()
    
    # Update html pull for forms
    shiny::observeEvent(input$page_update,{
      page_out <- r$remDr$getPageSource()
      page_out <- rvest::read_html(page_out[[1]])
      r$page_html <- page_out
    })
    
    # update tag form
    shiny::observeEvent(r$page_html, {
      tag_vec <- unique_tags(r$page_html)
      
      output$tag_select <- shiny::renderUI(
        shiny::selectInput(ns("tag_select"), "Tag", choices = append("(No Tag)", tag_vec), selected = "(No Tag)")
      )
    })
    
    # update class form
    shiny::observeEvent(input$tag_select,{
      shiny::req(input$tag_select)
      if(length(input$tag_select) > 0){
        if(input$tag_select == "(No Tag)"){
          class_vec <- unique_classes(r$page_html, tag_in = NULL)
        }else{
          class_vec <- unique_classes(r$page_html, tag_in = input$tag_select)
        }
      }else{
        class_vec <- c("(Error, Tag too short!)")
      }
      
      output$class_select <- shiny::renderUI(
        shiny::selectizeInput(ns("class_select"), "Class", choices = append("(No Class)", class_vec), selected = "(No Class)")
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
      output$attr_name_select <- shiny::renderUI({
        shiny::selectizeInput(ns("attr_name_select"), "Attribute Name", choices = append("(No Attribute)", attr), selected = "(No Attribute)")
      })
    })
    
    # update attr applicable values
    shiny::observeEvent(list(input$tag_select, input$class_select, input$attr_name_select),{
      shiny::req(input$tag_select)
      shiny::req(input$class_select)
      if(input$class_select == "(No Class)"){
        attr <- r$page_html %>% attr_values(tag_in = input$tag_select, attr_in = input$attr_name_select)
      }else{
        attr <- r$page_html %>% attr_values(tag_in = input$tag_select, class_in = input$class_select, attr_in = input$attr_name_select)
      }
      output$attr_value_select <- shiny::renderUI({
        shiny::selectizeInput(ns("attr_value_select"), "Attribute Value", choices = append("(No Value)", attr), selected = "(No Value)")
      })
    })
    
    # append var for tag when constructing css selector
    observeEvent(input$tag_add, {
      if(input$tag_select != "(No Tag)"){
        css$tag <- input$tag_select
      }else{
        css$tag <- NULL
      }
    })
    
    observeEvent(input$tag_remove, {
      css$tag <- NULL
    }) 
    
    # append var for class when constructing css selector
    observeEvent(input$class_add, {
      if(input$class_select != "(No Class)"){
        if(is.null(css$class)){
          css$class <- paste0(".",input$class_select)
        }else{
          css$class <- paste0(css$class, ".",input$class_select)
        }
      }
    }) 
    
    observeEvent(input$class_remove, {
      if(input$class_add){
        css$class <- stringr::str_remove(css$class, pattern = paste0(".",input$class_select))
      }
    })
    
    # append var for attributes when constructing css selector
    observeEvent(input$attr_add, {
      
      if(input$attr_name_select != "(No Attribute)" & input$attr_value_select == "(No Value)"){
        attr_in <- paste0("[",input$attr_name_select,"]")
      }
      
      if(input$attr_name_select != "(No Attribute)" & input$attr_value_select != "(No Value)"){
        attr_in <- paste0("[",input$attr_name_select," = '", input$attr_value_select, "']")
      }
      
      if(is.null(css$attr)){
        css$attr <- attr_in
      }else{
        css$attr <- paste0(css$attr, attr_in)
      }
    })
    
    observeEvent(input$attr_remove, {
      
      if(input$attr_name_select != "(No Attribute)" & input$attr_value_select == "(No Value)"){
        attr_out <- paste0("\\[",input$attr_name_select,"\\]")
      }
      
      if(input$attr_name_select != "(No Attribute)" & input$attr_value_select != "(No Value)"){
        attr_out <- paste0("\\[",input$attr_name_select," = '", input$attr_value_select, "'\\]")
      }
      
      css$attr <- stringr::str_remove(css$attr, pattern = attr_out)
      
    })
    
    # UI for CSS selector, constructing the selector
    observeEvent(list(css$tag, css$class, css$attr), {
      sel_in <- paste0(css$tag, css$class, css$attr)
      print(sel_in)
      output$css_select_ui <- renderUI({
        shiny::textInput(ns("css_select"), "CSS Selector", value = sel_in)
      })
    })
    
    # Adds the red boxin for the CSS selected.
    observeEvent(input$css_select,{
      
      css_selector <- input$css_select

      if(length(css_selector) > 0){
      
      if(!is.null(r$last_css)){
        # Removing previous selections if/when they exist
        js <- paste0("document.querySelectorAll('",r$last_css,"')")
        js <- 
          paste0(js,
                 ".forEach(el => el.style.border = '');"
          )
        
        try(r$remDr$executeScript(js, args = list(NULL)))
      }
      
      js <- paste0("document.querySelectorAll('",css_selector,"')")
      js <- 
        paste0(js,
          ".forEach(el => el.style.border = '2px solid red');"
        )
      try(r$remDr$executeScript(js, args = list(NULL)))
      
      r$last_css <- css_selector
      
      }
    })
    
    # Removes the red boxin for the css selected.
    observeEvent(input$remove,{
      
      try(
        if(!is.null(r$last_css)){
          js <- paste0("document.querySelectorAll('",r$last_css,"')")
          js <- 
            paste0(js,
                 ".forEach(el => el.style.border = '');"
            )
          r$remDr$executeScript(js, args = list(NULL))
          r$last_css <- NULL
        }
      )
      
      css$tag <- NULL
      css$class <- NULL
      css$attr <- NULL
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
