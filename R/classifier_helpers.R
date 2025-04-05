#' Webglace::selector
#'
#' An R6 class allowing static and dynamic webpage scraping and interaction.
#' Allows for selection of elements using hierarchal CSS selectors or XPath for JS injection.
#'
#' @export
selector <- R6::R6Class(
  
  classname = "selector",
  private = list(),
  public = list(
    
    css_self = NULL,
    css_within = NULL,
    css_contains = NULL,
    #text_self = NULL,
    
#' @param css_self CSS selector targeting the main element.
#' @param css_within CSS selector indentifying parent containers
#' @param css_contains CSS selector identifying the presence of child elements
#'
#' @return A webglace::selector class object
#' @export
#'
#' @examples Webglace::selector$new(css_self = "*", css_within = "div.class1", css_contains = "img.class2")
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
    
#' @param css_in CSS selector string.
#'
#' @return The front tag portion of a css selector
#' @export
#'
#' @examples selector$get_tag(css_in = "*")
    get_tag = function(css_in){
      
      tag <- css_in %>% 
        stringr::str_extract("[A-Za-z0-9\\-\\_]*[\\.\\[]") %>% 
        stringr::str_remove("\\.") %>% 
        stringr::str_remove("\\[")
      
      if(!is.na(tag)){
        if(tag == ""){
          tag <- "*"
        }
      }
      
      if(!stringr::str_detect(css_in, "\\.|\\[")){
        tag <- css_in
      }
      
      return(tag)
    },
    
#' @param css_in CSS selector string.
#'
#' @return A list of css selector classes
#' @export
#'
#' @examples selector$get_classes(css_in = "*")
    get_classes = function(css_in){
      
      classes <- 
        css_in %>% 
        stringr::str_extract_all("\\.[A-Za-z0-9\\-\\_]+(?=[\\.\\[])|\\.[A-Za-z0-9\\-\\_]+$")
      
      classes_out <- list()
      
      for(c in classes){
        c_out <- c %>% 
          stringr::str_remove_all("\\.") %>% 
          stringr::str_remove("\\[")
        
        classes_out <- append(classes_out, c_out)
      }
      
      classes <- classes_out
      
      if(!is.list(classes)){
        if(classes == "character(0)"){
          classes <- NULL
        }
      }
      
      if(length(classes) == 0){
        classes <- NULL
      }
      
      return(classes)
    },

#' @param css_in CSS selector string.
#'
#' @return A list of css selector attributes
#' @export 
#'
#' @examples selector$get_attr(css_in = "*")
    get_attr = function(css_in){
      
      # print("ATTR Prior")
      attr_out <- css_in %>% stringr::str_extract_all("\\[[A-Za-z0-9 \\'\\=\\-\\_\\(\\)\\;]*\\]")
      # print("Attr After")
      # print(attr_out)
      
      # print(str(attr_out))
      
      if(length(attr_out) == 0){
        # print("l0")
        attr_out <- NULL
      }else{
        # print("l>0")
        if(length(attr_out[[1]]) == 0){
          attr_out <- NULL
        }
      }
      
      return(attr_out)
    },
    
#' @param css_in CSS selector string.
#'
#' @return Class list ready for xpath parsing
#' @export
#'
#' @examples selector$xpath_classes(css_in = "*")
    xpath_classes = function(css_in){
      
      classes <- self$get_classes(css_in)
      class_buffer <- list()
      
      if(!is.null(classes)){
        for(c in classes){
          c_out <- paste0("contains(concat(' ', normalize-space(@class), ' '), ' ", c, " ')")
          class_buffer <- append(class_buffer, c_out)
        }
      }
      
      return(class_buffer)
    },
    
#' @param css_in CSS selector string.
#'
#' @return Attribute list ready for xpath parsing
#' @export
#'
#' @examples selector$xpath_attrs(css_in = "*")
    xpath_attrs = function(css_in){
      
      attributes <- self$get_attr(css_in)
      # print("ATTRIBUTES BEFORE CONVERSION")
      # print(attributes)
      
      attr_buffer <- list()
      
      if(!is.null(attributes)){
        for(a in attributes){
          a_out <- stringr::str_remove_all(a, pattern = "\\[|\\]")
          a_out <- paste0("@", trimws(a_out))
          attr_buffer <- append(attr_buffer, a_out)
        }
      }
      
      return(attr_buffer)
      
    },

#' @return A formatted xpath with ancestor and descendants included. Tags classes and attributes handled. May dislike special characters.
#' @export
#'
#' @examples selector$xpath()
    xpath = function(){
      
      xpath_out <- "//"
      
      # Main
      if(is.null(self$css_self)){
        xpath_out <- paste0(xpath_out, "*")
      }else{
        xpath_out <- paste0(xpath_out, self$get_tag(self$css_self))
      }
      
      xpath_out <- paste0(xpath_out, "[")
      
      # Adding Classes
      
      c_a_buffer <- list()
      c_a_buffer <- append(c_a_buffer, self$xpath_classes(self$css_self))
      
      # print("CSS ATTRS SELF")
      # print(self$xpath_attrs(self$css_self))
      
      c_a_buffer <- append(c_a_buffer, self$xpath_attrs(self$css_self))
      
      c_a_buffer <- paste0(c_a_buffer, collapse = " and ")
      
      ancestor_buffer <- list()
      ancestor_buffer <- append(ancestor_buffer, self$xpath_classes(self$css_within))
      ancestor_buffer <- append(ancestor_buffer, self$xpath_attrs(self$css_within))
      ancestor_buffer <- paste0(ancestor_buffer, collapse = " and ")
      
      if(is.null(self$css_within)){
        a_tag <- paste0("*")
      }else{
        a_tag <- paste0(self$get_tag(self$css_within))
      }
      
      ancestor_buffer <- paste0("ancestor::", a_tag, "[", ancestor_buffer, "]")
      
      desc_buffer <- list()
      desc_buffer <- append(desc_buffer, self$xpath_classes(self$css_contains))
      desc_buffer <- append(desc_buffer, self$xpath_attrs(self$css_contains))
      desc_buffer <- paste0(desc_buffer, collapse = " and ")
      
      if(is.null(self$css_contains)){
        d_tag <- paste0("*")
      }else{
        d_tag <- paste0(self$get_tag(self$css_contains))
      }
      
      desc_buffer <- paste0("descendant::", d_tag, "[", desc_buffer, "]")
      
      
      inner_buffer <- paste(c_a_buffer, ancestor_buffer, desc_buffer, sep = " and ")
      
      xpath_out <- paste0(xpath_out, inner_buffer)
      
      xpath_out <- paste0(xpath_out, "]")
      
      xpath_out <- stringr::str_remove_all(
        xpath_out, pattern = 
        " and descendant\\:\\:\\*\\[\\]| and ancestor\\:\\:\\*\\[\\]| descendant\\:\\:\\*\\[\\]| ancestor\\:\\:\\*\\[\\]")
      xpath_out <- stringr::str_replace(xpath_out, pattern = "\\[ and ", replacement = "\\[")
      xpath_out <- stringr::str_remove_all(xpath_out, pattern = "\\[\\]")
      
      # print("MAIN IS")
      # print(self$css_self)
      
      # print("XPATH IS")
      # print(xpath_out)
      
      return(xpath_out)
    },
    
#' @param html_in An HTML node tree via rvest or XML.
#'
#' @return A filtered HTML node tree
#' @export
#'
#' @examples selector$scrape(html_in = page[[1]])
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
    
#' @param border A TRUE/FALSE specifying whether to add or remove the red border highlight on a webpage.
#'
#' @return A JS script string
#' @export
#'
#' @examples selector$js(border = TRUE)
    js = function(border = TRUE){
      
      # Methods for flagging whether various css selectors match to adjacent/nested elements
      # print(str(self$css_self))
      
      if(!is.null(self$css_self)){
        if(trimws(self$css_self) != ""){
        js_self <- paste0("let elements = document.querySelectorAll(\"",self$css_self,"\");")
        }else{
          js_self <- paste0("let elements = document.querySelectorAll(\"","*","\");")
        }
      }else{
        js_self <- paste0("let elements = document.querySelectorAll(\"","*","\");")
      }
      
      js_contains <- paste0("let contains = el.querySelector(\"", self$css_contains, "\") !== null;")
      js_within <- paste0("let within = el.closest(\"", self$css_within,"\") !== null;")
      
      logical <- c()
      js_query <- paste(js_self, "elements.forEach(el => {")
    
      # We need to append both logical identification and control structure to respond to css selectors, but only when provided
      if(!is.null(self$css_contains)){
        if(trimws(self$css_contains) != ""){
          logical <- append(logical, "contains")
          js_query <- paste(js_query, js_contains)
        }
      }
      
      if(!is.null(self$css_within)){
        if(trimws(self$css_within) != "" & !is.null(self$css_within)){
          logical <- append(logical, "within")
          js_query <- paste(js_query, js_within)
        }
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
        

        try(
          if(trimws(self$css_contains) != "" & !is.null(self$css_contains)){
            pass <- TRUE
          }
        )
        
        try(
          if(trimws(self$css_within) != "" & !is.null(self$css_within)){
            pass <- TRUE
          }
        )

        
        if(pass){
          js_query <- paste0(js_query, "if(", logical, "){el.style.border = ", border,"}});")
        }else{
          js_query <- paste0(js_query, "el.style.border = ", border, "})")
        }
      
      return(js_query)
    },
    
#' @param html_in An rvest html node tree.
#' @param rm.na A TRUE/FALSE for removing NA href values.
#' 
#' @return An href list, useful for URL based recursion.
#' @export
#'
#' @examples selector$href(html_in = page[[1]], na.rm = TRUE)
    href = function(html_in, rm.na = TRUE){
      
      href_out <- html_in %>% 
        self$scrape() %>% 
        rvest::html_attr("href")
      
      if(rm.na){
        href_out <- purrr::discard(href_out, is.na)
      }
      
      return(href_out)
      
    },
    
#' @param html_in An revest style html node tree
#' 
#' @return A list of text strings matched to nodes within the html using selectors.
#' @export
#'
#' @examples selector$text(html_in = page[[1]])
    text = function(html_in){
      
      text_out <- html_in %>% 
        self$scrape() %>% 
        rvest::html_text()
      
      return(text_out)
      
    },

#' @param remDr A selenium webdriver address
#' @param text A string, which if included is required to be present on the element to be clicked.
#' @param exact_text A logical, specifying whether substring or exact matching is to be used.
#' @param offset An integer allowing you to click on the second, third matching element etc.
#' @param scroll_time An integer, in seconds, specifying how long to spend scroling to the next element before continuing. Allows for offpage elements to be made visible before interaction.
#' @param strict A logical, specifying whether to use strict visibility constraints on user inputs.
#' 
#' @return NULL
#' @export
#'
#' @examples selector$click(
#'   remDr = remDr, 
#'   text = "my_button_text", 
#'   exact_text = TRUE, 
#'   offset = 2, 
#'   scroll_time = 3, 
#'   strict = FALSE
#' )
    click = function(remDr, text = NULL, exact_text = FALSE, offset = NULL, scroll_time = NULL, strict = TRUE){
      
      if(is.null(text)){
        path <- self$xpath()
      }else{
        path <- self$xpath_text(text = text, exact = exact_text)
      }
      
      if(is.null(offset)){
        
        e <- remDr$findElement(using = "xpath", value = path)
        remDr$executeScript("arguments[0].scrollIntoView({ behavior: 'smooth', block: 'center' });", list(e))
        if(!is.null(scroll_time)){
          Sys.sleep(scroll_time)
        }
        if(strict){
          e$clickElement()
        }else{
          remDr$executeScript("arguments[0].click();", list(e))
        }
        
      }else{
        
        e <- remDr$findElements(using = "xpath", value = path)
        
        if(offset <= length(e)){
        
          remDr$executeScript("arguments[0].scrollIntoView({ behavior: 'smooth', block: 'center' });", list(e[[offset]]))
          if(!is.null(scroll_time)){
            Sys.sleep(scroll_time)
          }
          
          if(strict){
            e[[offset]]$clickElement()
          }else{
            remDr$executeScript("arguments[0].click();", list(e[[offset]]))
          }
        
        }
      }
      
    },
    
#' @param remDr A selenium webdriver address
#' @param px Number of pixels to scroll down within the main window.
#' 
#' @return NULL
#' @export
#'
#' @examples selector$scroll(remDr = remDr, px = 1000)
    scroll = function(remDr, px = 500){
      
      e <- remDr$findElement(using = "xpath", value = self$xpath())
      remDr$executeScript(paste0("arguments[0].scrollBy({top:", px, ", behavior: 'smooth'});"), list(e))
      
    },
    

#' @param text A string to match
#' @param exact Exact String match? TRUE/FALSE
#' @param xpath_in A string (optional) containing the xpath you want to injuect text matching into. By defaul, pulls from the internal selectors.
#' 
#' @return An xpath string
#' @export
#'
#' @examples selector$xpath_text(
#'   text = "match_my_text", 
#'   exact = FALSE,
#'   xpath_in = "my_custom_xpath"
#'  )
    xpath_text = function(text, exact = FALSE, xpath_in = NULL){
      
      if(!is.null(xpath_in)){
        xpath <- xpath_in
      }else{
        xpath <- self$xpath()
      }
      
      if(exact){
        xpath_inject <- paste0("text()='",text,"'")
      }else{
        xpath_inject <- paste0("contains(text(), '",text,"')")
      }
      
      if(stringr::str_detect(xpath, "\\[")){
        # By definition of prior xpath function
        xpath <- stringr::str_replace(xpath, pattern = "\\[", replacement = paste0("[", xpath_inject, " and "))
      }else{
        xpath <- paste0(xpath, "[", xpath_inject, "]")
      }
      
      return(xpath)
      
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

# Attribute helper function for population the selector construction form in 
# mod_classification_1

attr_names <- function(html_in, tag_in, class_in = NULL){
  
  # print(tag_in)
  
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

# Attribute value helper function for population the selector construction form in
# mod_classification_1

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

# Smooth scroll function, internal
smooth_scroll <- function(remDr, px){
  js <- paste0("window.scrollBy({top: ", px,", behavior: 'smooth'});")
  remDr$executeScript(js)
}

# Function for internal demo development, simplifying the data fetch process.
get_page <- function(remDr){
  page <- remDr$getPageSource()
  page <- rvest::read_html(page[[1]])
  return(page)
}
