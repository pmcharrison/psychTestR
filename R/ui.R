ui <- function(opt) {
  title_content <- shiny::wellPanel(shiny::h4(opt$title, align = "center"),
                                    style = "padding: 3px;")
  main_content <- shiny::wellPanel(align = "center", shiny::uiOutput("ui"))

  shiny::fluidPage(
    theme = opt$theme,
    title = opt$title,
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
                      shiny::p(htmltools::htmlEscape(opt$problems_info, FALSE)),
                      shiny::uiOutput("admin_panel.ui"),
                      admin_panel.modals
                    ))),
    include_scripts())
}

include_scripts <- function() {
  scripts <- c(
    "js/push-p-id-to-url.js",
    "js/reset-p-id-and-refresh-browser.js",
    "js/confirm-resume-session.js",
    "js/confirm-delete-results.js",
    "js/confirm-delete-errors.js",
    "js/confirm-clear-sessions.js",
    "js/hide-content.js",
    "js/trigger-button.js",
    "js/next-page.js"
  )
  lapply(scripts, function(x) shiny::includeScript(system.file(
    x, package = "psychTest")))
}
