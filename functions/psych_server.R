shinyPsych_server <- function(params) {
  function(input, output) {
    
    output$ui <- renderUI({
      fluidPage(
      titlePanel(params$title),
      withTags({
        video(source(src = "test.mp4",
                     type = "video/mp4"),
              width = "25%",
              autoplay = "autoplay")
      }),
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = if (is.null(input$bins)) 30 else input$bins),
      plotOutput("distPlot"))
    })

    output$distPlot <- renderPlot({

      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2]
      bins <- seq(min(x), max(x), length.out = input$bins + 1)

      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')

    })
  }
}