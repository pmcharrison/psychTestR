psychTestUI  <- function(params) {
  title_content <- wellPanel(h2(params$title, align = "center"))
  main_content <- wellPanel(align = "center", uiOutput("ui"))
  
  fluidPage(fluidRow(column(12, title_content),
    fluidRow(column(2),
             column(8, main_content)),
             column(2)))
}