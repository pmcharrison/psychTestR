source("scripts/load_code.R")

server <- shinyPsych_server(params)
ui <- shinyPsych_UI(params)

shinyApp(ui = ui, server = server)