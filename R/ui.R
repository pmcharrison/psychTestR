ui <- function(opt) {
  header <- if (opt$display$show_header) {
    shiny::fluidRow(shiny::column(12, shiny::wellPanel(
      style = paste("padding: 3px; padding-left: 10px; padding-right: 10px;",
                    "background-color: white;"),
      shiny::div(
        style = paste("display: flex; justify-content: space-between;",
                      "align-items: center;"),
        shiny::uiOutput("title"),
        if (!is.null(opt$logo)) {
          shiny::img(
            src = opt$logo,
            style = sprintf("width: %s; height: %s;",
                            width = opt$logo_width,
                            height = opt$logo_height))
        } else " "
      )
    )))
  }

  content <- shiny::fluidRow(
    id = "content",
    if (opt$display$left_margin > 0) shiny::column(opt$display$left_margin),
    shiny::column(
      12L - opt$display$left_margin - opt$display$right_margin,
      style = "padding-left: 0px; padding-right: 0px;",
      shiny::wellPanel(
        align = "center",
        style = paste0(
          "background-color: ", opt$display$content_background_colour, "; ",
          "margin: 0px; ",
          "border: ", opt$display$content_border
        ),
        shiny::uiOutput("ui")
      )),
    if (opt$display$right_margin > 0) shiny::column(opt$display$right_margin)
  )

  footer <- shiny::div(
    # hidden = !opt$display$show_footer,
    id = "footer",
    hidden = if (opt$display$show_footer) NULL else "hidden",
    shiny::fluidRow(shiny::column(
      12,
      shiny::tags$div(
        style = "padding: 10px",
        align = "center",
        shiny::p(shiny::uiOutput("problems_info")),
        if (opt$display$admin_panel) {
          shiny::tags$div(
            shiny::uiOutput("admin_panel.ui"),
            admin_panel.modals
          )
        }
      ))))

  shiny::fluidPage(
    theme = opt$theme,
    lapply(opt$display$css, shiny::includeCSS),
    shinyjs::useShinyjs(),
    header,
    content,
    footer,
    include_scripts(),
    include_js_opt(opt)
  )
}

include_js_opt <- function(opt) {
  shiny::tags$script(sprintf("var test_options = JSON.parse('%s');",
                             jsonlite::toJSON(opt$js_opt, auto_unbox = TRUE)))

}

include_scripts <- function(opt) {

  scripts <- c(
    "js/confirm-clear-sessions.js",
    "js/confirm-delete-errors.js",
    "js/confirm-delete-results.js",
    "js/confirm-resume-session.js",
    "js/hide-content.js",
    "js/next-page.js",
    "js/reset-p-id-and-refresh-browser.js",
    "js/show-footer.js",
    "js/trigger-button.js",
    "js/navigate-away.js"
  )

  if(!is.null(opt$additional_scripts)) {
    scripts <- c(scripts, opt$additional_scripts)
  }

  lapply(scripts, function(x) shiny::includeScript(system.file(
    x, package = "psychTestR")))
}
