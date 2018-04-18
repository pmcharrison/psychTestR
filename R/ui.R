ui  <- function(title, options) {
  stopifnot(is.scalar.character(title))
  title_content <- shiny::wellPanel(shiny::h3(title, align = "center"))
  main_content <- shiny::wellPanel(align = "center", shiny::uiOutput("ui"))
  footer_content <- shiny::uiOutput("footer")

  function(request) {
    shiny::fluidPage(
      theme = options$theme,
      title = title,
      shinyjs::useShinyjs(),
      shiny::includeScript(system.file("js/session.js",
                                       package = "psychTest")),
      shiny::includeScript(system.file("js/request-new-session.js",
                                       package = "psychTest")),
      shiny::includeScript(system.file("js/confirm-resume-session.js",
                                       package = "psychTest")),
      # shiny::tags$head(shiny::tags$script(src = "session.js")),
      shiny::uiOutput("modals"),
      shiny::fluidRow(shiny::column(12, title_content)),
      shiny::fluidRow(
        shiny::column(2),
        shiny::column(8, main_content),
        shiny::column(2, shiny::tags$div(style = "padding: 10px",
                                         align = "center",
                                         shiny::uiOutput("side_panel_ui")))
      ),
      shiny::fluidRow(shiny::column(12, footer_content)))
  }
}
