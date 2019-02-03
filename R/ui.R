ui <- function(opt) {
  title_content <- shiny::wellPanel(
    style = "padding: 3px; padding-left: 10px; padding-right: 10px; background-color: white;",
    shiny::div(
      style = "display: flex; justify-content: space-between; align-items: center;",
      shiny::h4(opt$title),
      if (!is.null(opt$logo)) {
        shiny::img(
          src = opt$logo,
          style = sprintf("width: %s; height: %s;",
                          width = opt$logo_width,
                          height = opt$logo_height))
      } else " "
      )
    )
  main_content <- shiny::wellPanel(align = "center",
                                   style = "background-color: white",
                                   shiny::uiOutput("ui"))

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
    "js/confirm-clear-sessions.js",
    "js/confirm-delete-errors.js",
    "js/confirm-delete-results.js",
    "js/confirm-resume-session.js",
    "js/hide-content.js",
    "js/next-page.js",
    "js/reset-p-id-and-refresh-browser.js",
    "js/trigger-button.js"
  )
  lapply(scripts, function(x) shiny::includeScript(system.file(
    x, package = "psychTestR")))
}
