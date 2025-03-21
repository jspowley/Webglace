selector <- R6::R6Class(
  
  classname = "selector",
  private = list(),
  public = list(
    
    css_self = NULL,
    css_within = NULL,
    css_contains = NULL,
    #text_self = NULL,
    
    initialize = function(
      css_self = NULL, 
      css_within = NULL, 
      css_contains = NULL
      #text_self = NULL
    ){
      
      self$css_self <- css_self
      self$css_within <- css_within
      self$css_contains <- css_contains
      #self$text_self <- text_self
      
    },
    
    scrape = function(html_in){
      
      # Filter for 'contained within' css class
      if(!is.null(self$css_within)){
        nodes <- html_in %>% 
          rvest::html_nodes(self$css_within)
        nodes <- nodes %>% rvest::html_nodes(self$css_self)
        
      }else{
        nodes <- html_in %>% rvest::html_nodes(self$css_self)
      }
      
      # Filtering at the selector level for text contents, removed due to JS limitations with REGEX a feature which can be moved downstream quite easily.
      #if(!is.null(self$text_self)){
      #  text_vec <- nodes %>% rvest::html_text()
      #  text_vec <- text_vec %>% stringr::str_detect(pattern = self$text_self)
      #  nodes <- nodes[text_vec]
      #}
      
      # Filtering for only css_self tags which contain css_contains tags
      if(!is.null(self$css_contains)){
        node_select <- c()
        
        for(n in nodes){
          subset <- n %>% rvest::html_elements(self$css_contains)
          if(length(subset) > 0){
            node_select <- append(node_select, TRUE)
          }else{
            node_select <- append(node_select, FALSE)
          }
        }
        nodes <- nodes[node_select]
      }
      return(nodes)
    },
    
    js = function(border = TRUE){
      
      # Methods for flagging whether various css selectors match to adjacent/nested elements
      print(str(self$css_self))
      
      if(!is.null(self$css_self)){
        if(trimws(self$css_self) != ""){
        js_self <- paste0("let elements = document.querySelectorAll('",self$css_self,"');")
        }else{
          js_self <- paste0("let elements = document.querySelectorAll('","*","');")
        }
      }else{
        js_self <- paste0("let elements = document.querySelectorAll('","*","');")
      }
      
      js_contains <- paste0("let contains = el.querySelector('", self$css_contains, "') !== null;")
      js_within <- paste0("let within = el.closest('", self$css_within,"') !== null;")
      
      logical <- c()
      js_query <- paste(js_self, "elements.forEach(el => {")
    
      # We need to append both logical identification and control structure to respond to css selectors, but only when provided
      if(!is.null(self$css_contains)){
        logical <- append(logical, "contains")
        js_query <- paste(js_query, js_contains)
      }
      
      if(!is.null(self$css_within)){
        logical <- append(logical, "within")
        js_query <- paste(js_query, js_within)
      }
      
      logical <- paste(logical, collapse = " && ")
      
      # Are we highlighting the border or removing?
      if(border){
        border <- "'2px solid red'"
      }else{
        border <- "''"
      }
      
      
      # Standard or conditional query?
        
        pass <- FALSE
        

          if(trimws(self$css_contains) != "" ){
            pass <- TRUE
          }
        

          if(trimws(self$css_within) != ""){
            pass <- TRUE
          }

        
        if(pass){
          js_query <- paste0(js_query, "if(", logical, "){el.style.border = ", border,"}});")
        }else{
          js_query <- paste0(js_query, "el.style.border = ", border, "})")
        }
      
      return(js_query)
    }

  ))

# For finding unique tag types within the page.
unique_tags <- function(html_in){
  html_in %>% 
    xml2::xml_find_all("//*") %>% 
    xml2::xml_name() %>% 
    unique() %>% 
    sort() %>% 
    return()
}

readRDS("test_page.rds") %>% 
  rvest::read_html() %>% 
  unique_tags()

s <- selector$new("li.card_card___ZlNq", "ul.cta-cards_cards__ApWvd", "img")
nodes <- s$scrape(readRDS("test_page.rds") %>% rvest::read_html())

nodes

s$js()

unique_classes <- function(html_in, tag_in = NULL){
  
  css_selector <- ifelse(is.null(tag_in), "*", tag_in)
  
  html_in %>%
    rvest::html_nodes(css_selector) %>% 
    rvest::html_attrs() %>% 
    unlist() %>% 
    .[names(.) == "class"] %>% 
    strsplit(" ") %>% 
    unlist() %>% 
    unique() %>% 
    sort() %>% 
    return()
}

attr_names <- function(html_in, tag_in, class_in = NULL){
  
  print(tag_in)
  
  if(tag_in == "(No Tag)"){
    tag_in <- "*"
  }
  
  css_id <- ifelse(is.null(class_in), tag_in, paste0(tag_in,".",class_in))
  
  if(is.null(css_id)){
    css_id <- "*"
  }

  html_in %>%
    rvest::html_nodes(css_id) %>% 
    rvest::html_attrs() %>% 
    unlist() %>% 
    names() %>% 
    .[. != "class"] %>% 
    unique() %>% 
    sort() %>% 
    return()
  
}

readRDS("test_page.rds") %>% 
  rvest::read_html() %>% 
  rvest::html_nodes("*")

attr_values <- function(html_in, tag_in = NULL, attr_in, class_in = NULL){
  
  if(tag_in == "(No Tag)"){
    tag_in <- "*"
  }
  
  css_id <- ifelse(is.null(class_in), tag_in, paste0(tag_in,".",class_in))
  
  if(is.null(css_id)){
    css_id <- "*"
  }
  
  html_in %>%
    rvest::html_nodes(css_id) %>% 
    rvest::html_attrs() %>% 
    unlist() %>% 
    .[names(.) == attr_in] %>% 
    unique() %>% 
    sort() %>% 
    as.character() %>% 
    return()
}

readRDS("test_page.rds") %>% 
  rvest::read_html() %>% 
  rvest::html_nodes("*.text")

for(n in nodes){
  n %>% rvest::html_node("a") %>% length() %>% print()
}
