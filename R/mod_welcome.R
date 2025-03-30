#' welcome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_welcome_ui <- function(id) {
  
  ns <- NS(id)
  
  #tagList(
  
  bslib::nav_panel(
    title = "Welcome",
      
    bslib::page_fluid(
        
        shiny::div(
          
          shiny::img(src = "www/logo.png", style = "float: left; margin-right: 15px; width: 350px; padding: 20px;")
        
          ),
        
        shiny::div(
          
          style = "padding: 20px;",
          h1("Welcome!"),
          h2("About"),
          p("Webglace is a web scraping app designed to allow rapid webscraping prototyping and orchestration. The intention behind this project was very clear: To remove frictions from the process of prototyping a webscraping tool, and provide live feedback to the designer as they build their workflow."),
          p("A broader vision for this tool is the ability to web scrape serially. With minimized technical resistance to the webscraping process, it should become very easy to add to the information collection tech stack, and build a broad range of market intelligence services. Although selenium may not be ideal after a certain critical mass, where the variable cost of a heavier non NodeJS process outweighs the benefits of faster deployment, support via transparent XPath and CSS selector configuration allows the user to deploy in a more professional level of implementation."),
          h2("Browser"),
          p("The app is built to interact with a noVNC viewer and Selenium instance launched exposed on a docker network. In the browser session, this is where you can set up the Selenium linked browser session. Launch a browser, send it a starting link, test basic scraping, and download HTML. Scraping previews require the HTML to be actively pulled into memory using the 'Pull HTMl' button."),
          p("Using open source frameworks in orchestration comes with its quirks. Scrolling while hovering over the desktop in the browser viewer will switch between 1 of 4 default workspace sessions. If you find you have suddenly lost your browser, you can confirm your workspace session and change it by scrolling to the bottom left of the viewer."),
          h2("CSS Classification"),
          p("The CSS classification menu allows users to build, preview, and export CSS selectors. Selectors have the ability to scrape using parent, self, and child nodes built using CSS, conversion methods to parse with indetical identification properties into XPath, and have methods avaialble to accept and automatically scrape different kinds of data from an XML or HTML node tree."),
          p("CSS is used for the user selection menu since it is much more intuitive and approachable than XPath. A user can select a single tag, any number of classes, any number of attributes, and values for those attributes for a CSS selector. From here, this selector can be assigned as a parent, at the main scraping level, or as a child. Selectors exported with parent level selection logic while require that the main selector also be nested within the parent class, and a child selector will require contents nested underneath the scraped level in the tree to also be contained. This functionality allows for more robust and specific identification, which is helpful for dynamic scraping later on. Note that of tag, class, attributes, and values, all are optional, but at least one is required."),
          p("As you build a CSS selector, it will preview pattern recognition live within the web browser. Once parent, main, or child CSS identification terms have been added, the whole selector can be be tested using the 'Test Selector' button. From here, you can name your selector and export it to the 'Testing and Export' screen or discard the selector. Each selector will only export if the name provided is not currently in use."),
          h2("Testing and Export"),
          p("Each exported selector will appear as a card in the screen. You can test scraping all content, pulling text, urls via the href attribute, and button clicks through Selenium. You can also view the selectors configuration and feature documentation by viewing propoerties, or download each selector individually as desired. When testing scraping, both XPath and CSS selection logic outputs are shown for comparison and consisteny purposes. When testing button presses via 'Click Element', ensure you are viewing browser with 'View Browser'."),
          p("'Download All' downloads all selectors as a zip file."),
          h2("Other Tabs"),
          p("Webglace isn't just abou solving technical boundaries and barriers to entry. It's about improving the pace and quality of life of those engaging in research and market analytics,and those looking to take on more ambitious projects after hours when time is scarce and uick pivots are necessary. It's an internal tool, which assumes the user already has some familiarity with the web scraping process. Because of this, the design is minimalistic. It's a complex product, and as a result, if you really try to break it, there's very likely a way. To really weigh the tools value, nd ensure it's feature set is robust enough for what it was desinged to accomplish, I set out within the last week of deployment for this project to put together 'mini projects' which make use of this tool to structure the information collection. Each project includes a timeline marker indicating development time, a button allowing you to see the webscraping in action, and data collected as proof of concept. This approach should also allow me to stumble into shortcomings, and expand the selector class further."),
          h2("Theming"),
          p("This tool is designed for prototyping, and existing R Developers. As such, no excessive theming outside of stock Shiny is used, expect for slight color adjustments, ensuring allignment is clean, and space is used efficiently. That being said, since bslib is used as he main framework, bootswatch theming can quickly adjust the look and feel of most elements on the page. My opinion is that formatted buttons lead to confusion. This app is a technical tool, and as such, can get quiet busy on the page. My hope is that anyone who is already head deep in understanding other websites formatting will appreciate a minimal style they are used to, so they can fully focus on the website in front of them as they build their workflows.")
    
        ),
        
        # Stolen from ChatGPT since I can't be bothered to design custom footers, and standard boiler plate feels right for this...
        div(
          style = "margin-top: 30px; padding: 10px 0; text-align: center; font-size: 12px; color: #888;",
          HTML(
            paste0(
              "Built with <a href='https://shiny.posit.co/' target='_blank' style='color: #888;'>Shiny</a> by ",
              "<a href='https://www.linkedin.com/in/powleyjustin' target='_blank' style='color: #888;'>Justin Powley</a> · ",
              "© ", format(Sys.Date(), "%Y"), " · ",
              "<a href='https://github.com/jspowley' target='_blank' style='color: #888;'>GitHub</a>"
            )
          )
        )
      )
    )
}
    
#' welcome Server Functions
#'
#' @noRd 
mod_welcome_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_welcome_ui("welcome_1")
    
## To be copied in the server
# mod_welcome_server("welcome_1")
