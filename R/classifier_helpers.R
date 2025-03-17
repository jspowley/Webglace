selector <- R6::R6Class(
  
  classname = "selector",
  private = list(),
  public = list(
    
    js = NULL,
    css_self = NULL,
    css_within = NULL,
    css_contains = NULL,
    text_within = NULL,
    text_self = NULL,
    
    initialize = function(
      css_self, 
      css_within = NULL, 
      css_contains = NULL,
      text_within = NULL,
      text_self = NULL
    ){
      
      self$css_self <- css_self
      self$css_within <- css_within
      self$css_contains <- css_contains
      self$text_within <- text_within
      self$text_self <- text_self
      
    },
    
    scrape = function(html_in){
      
      # Filter for 'contained within' css class
      if(!is.null(self$css_within)){
        nodes <- html_in %>% 
          rvest::html_nodes(self$css_within)
        nodes <- nodes %>% rvest::html_nodes(self$css_self)
        
        # Filter by higher level container text contents, which naturally includes text of any and all containers organized within
        if(!is.null(self$text_within)){
          text_vec <- nodes %>% rvest::html_text()
          text_vec <- text_vec %>% stringr::str_detect(pattern = self$text_within)
          nodes <- nodes[text_vec]
        }
        
      }else{
        nodes <- html_in %>% rvest::html_nodes(self$css_self)
      }
      
      # Filtering at the selector level for text contents
      if(!is.null(self$text_self)){
        text_vec <- nodes %>% rvest::html_text()
        text_vec <- text_vec %>% stringr::str_detect(pattern = self$text_self)
        nodes <- nodes[text_vec]
      }
      
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
    
    make_js = function(){
      
    },
    
    border = function(){
      
    },
    
    no_border = function(){
      
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

s <- selector$new("div", "div.flex", css_contains = "img", text_self = "a")
nodes <- s$scrape(readRDS("test_page.rds") %>% rvest::read_html())

nodes

for(n in nodes){
  n %>% rvest::html_node("a") %>% length() %>% print()
}
