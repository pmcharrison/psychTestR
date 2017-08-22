source("scripts/load_code.R")
options(shiny.error = browser)

server <- psychTestServer(params)
ui <- psychTestUI(params)

shinyApp(ui = ui, server = server)