psychTestUI  <- function(params) {
  title_content <- wellPanel(h3(params$title, align = "center"))
  main_content <- wellPanel(align = "center", uiOutput("ui"))
  footer_content <- uiOutput("footer")
  
  fluidPage(
    theme = params$display_options$theme,
    shinyjs::useShinyjs(),
    uiOutput("modals"),
    fluidRow(column(12, title_content)),
    fluidRow(column(2, tags$div(style = "padding: 10px",
                                align = "center",
                                uiOutput("side_panel_ui"))),
             column(8, main_content),
             column(2)),
    fluidRow(column(12, footer_content)))
}