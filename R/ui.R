ui <- function(title, options) {
  stopifnot(is.scalar.character(title))
  title_content <- shiny::wellPanel(shiny::h3(title, align = "center"))
  main_content <- shiny::wellPanel(align = "center", shiny::uiOutput("ui"))

  shiny::fluidPage(
    theme = options$theme,
    title = title,
    shinyjs::useShinyjs(),
    shiny::fluidRow(shiny::column(12, title_content)),
    shiny::fluidRow(
      id = "content",
      shiny::column(2),
      shiny::column(8, main_content),
      shiny::column(2)
    ),
    shiny::fluidRow(
      shiny::column(12,
                    shiny::tags$div(
                      style = "padding: 10px",
                      align = "center",
                      shiny::uiOutput("admin_panel.ui"),
                      admin_panel.modals,
                      shiny::p(htmltools::htmlEscape(options$problems_info, FALSE))
                    ))),
    shiny::includeScript(system.file("js/push-p-id-to-url.js",
                                     package = "psychTest")),
    shiny::includeScript(system.file("js/reset-p-id-and-refresh-browser.js",
                                     package = "psychTest")),
    shiny::includeScript(system.file("js/confirm-resume-session.js",
                                     package = "psychTest")),
    shiny::includeScript(system.file("js/confirm-delete-results.js",
                                     package = "psychTest")),
    shiny::includeScript(system.file("js/hide-content.js",
                                     package = "psychTest")))
}
