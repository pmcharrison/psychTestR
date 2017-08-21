nextPage <- function() {
  if (is(page_stack[[1]], "test_page")) {
    # Next thing on the stack is a test page
    ## Finalise the current page (to do)
    ## Move to the next page
    rv$current_page <- rv$test_stack[[1]]
    rv$test_stack <- rv$test_stack[- 1]
  } else {
    stop("Don't know how to deal with the next thing on the stack!")
  }
}

shinyPsych_server <- function(params) {
  function(input, output) {
    # rv will store the state of our test
    rv <- reactiveValues(test_stack = params$pages,
                         current_page = NULL)
    # Calling getNextPage here advances our test to the first page
    getNextPage(rv)
    
    # The UI will be updated every time the corresponding slot in rv gets updated
    output$ui <- renderUI({
      rv$current_page$ui
    })
    
    # This observer checks active triggers for getNextPage
    observe({
      triggers <- rv$current_page$triggers
      trigger_values <- lapply(triggers, 
                               function(x) input[[x]])
      triggers_fired <- lapply(trigger_values,
                               function(x) {
                                 if (is.numeric(x)) {
                                   x > 0
                                 } else {
                                   !is.null(x)
                                 }
                               })
      if (any(triggers_fired)) {
        getNextPage(rv)
      }
    })
  }
}

# output$ui_body <- renderUI({
#   bins <- if (is.null(input$bins)) 10 else input$bins
#   print(bins)
#   test <- rnorm(bins)
#   fluidPage(
#     withTags({
#       video(source(src = "test.mp4",
#                    type = "video/mp4"),
#             width = "25%",
#             autoplay = "autoplay")
#       # h3(input$bins)
#       renderText("bins")
#     }),
#     "hello",
#     checkboxInput("my_test", test, value = FALSE)
#   )
# })