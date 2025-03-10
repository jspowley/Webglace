library(shiny)
library(RSelenium)
library(bslib)

remDr <- remoteDriver(
    remoteServerAddr = "localhost",
    port = 4444L,
    browserName = "chrome"
)
remDr$open()

ui <- fluidPage(
    titlePanel("Selenium-Powered Browser"),
    actionButton("run_script", "Run Script"),
    tags$iframe(src = "http://localhost:6900/vnc.html", width = "100%", height = "1080px")
)

server <- function(input, output, session) {
    observeEvent(input$run_script,{

        print(remDr$getCurrentUrl())
        remDr$navigate("https://www.canada.ca/en/revenue-agency.html")

    })
}

shinyApp(ui, server)
