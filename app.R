PIAT <- TRUE
gold_MSI <- FALSE
options(shiny.error = browser)
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

shinyApp(ui = ui, server = server)
