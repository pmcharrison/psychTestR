# This allows us to load shinyBS under Imports rather than Depends

load_shinyBS_resources <- function() {
  shiny::addResourcePath("sbs", system.file("www", package = "shinyBS"))
}

.onLoad <- function(...) load_shinyBS_resources()
.onAttach <- function(...) load_shinyBS_resources()
