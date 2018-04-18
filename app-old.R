closed <- file.exists("closed.txt") # Make a text file called closed.txt to close the test
PIAT <- TRUE
gold_MSI <- FALSE
options(shiny.error = browser)
# Change the debug options if the server is running on Peter's local machine
if (Sys.info()["nodename"] == "Peters-MacBook-Pro.local") {
  options(shiny.error = browser) 
}

library(shiny)
if (closed) {
  shinyApp(
    ui = fluidPage(
      title = "Test closed",
      div(
        h2("Test closed"),
        p("Sorry, this test is no longer available to participate in.")
      )
    ),
    server = function(input, output, session) {
      output$test <- renderText("Nothing")
    }
  )
} else {
  if (PIAT) {
    source("tests/piat/load_piat.R")
  } else if (gold_MSI) {
    lapply(list.files("common_functions/", pattern = "*\\.R$", full.names = TRUE), source)
    source("tests/gold_msi/gold_msi.R")
    params <- new.env()
    source("tests/gold_msi/params.R", local = params)
  } else {
    library(shinyBS)
    library(shinyWidgets)
    lapply(list.files("common_functions/", pattern = "*\\.R$", full.names = TRUE), source)
    params <- new.env()
    source("tests/cat/test_cat.R", local = params)
  }
  server <- psychTestServer(params)
  ui <- psychTestUI(params)
  
  shinyApp(ui = ui, server = server,
           onStart = function() {
             onStop(function() params$server_quit_fun())})
}
