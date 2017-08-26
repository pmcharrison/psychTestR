psychTestUI  <- function(params) {
  title_content <- wellPanel(h2(params$title, align = "center"))
  main_content <- wellPanel(align = "center", uiOutput("ui"))
  
  fluidPage(
    theme = params$display_options$theme,
    useShinyjs(),
    fluidRow(column(12, title_content),
    fluidRow(column(2, params$side_panel_ui),
             column(8, main_content)),
             column(2)))
}