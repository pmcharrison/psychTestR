source("tests/piat/load_piat.R")
options(shiny.error = browser)

server <- psychTestServer(params)
ui <- psychTestUI(params)

shinyApp(ui = ui, server = server)