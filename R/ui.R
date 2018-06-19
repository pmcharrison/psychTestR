ui <- function(opt) {
  title_content <- shiny::wellPanel(
    style = "padding: 3px; padding-left: 10px; padding-right: 10px",
    shiny::div(
      style = "display: flex; justify-content: space-between; align-items: center;",
      if (!is.null(opt$logo)) {
            shiny::img(
              src = opt$logo,
              style = sprintf("width: %s; height: %s;",
                              width = opt$logo_width,
                              height = opt$logo_height))
          } else " ",
      shiny::h4(opt$title),
      shiny::h4("psychTestR")
      )
    )
    # shiny::fluidRow(
    #   shiny::column(4, if (!is.null(opt$logo)) {
    #     shiny::img(
    #       src = opt$logo,
    #       style = sprintf(
    #         paste0("display:block;margin-left:auto;margin-right:auto;",
    #                "width:%s;height:%s;",
    #                # "vertical-align:middle"),
    #                "position:relative;top:50%%;margin-top:-31px;"),
    #         width = opt$logo_width,
    #         height = opt$logo_height))
    #   }),
    #   shiny::column(4, shiny::h4(opt$title, align = "center")),
    #   shiny::column(4)
    # ))
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
