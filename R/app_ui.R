#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bslib::page_navbar(
      
      title = shiny::img(src = "www/logo.png", height = "80px"),
      
      mod_welcome_ui("welcome_1"),
      mod_browser_ui("browser_1"),
      mod_classification_1_ui("class_1")
      
    )
  )
}



#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  
  tags$head(
    golem::favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Webglace"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
