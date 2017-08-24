#### Top-level function ####

psychTestServer <- function(params) {
  function(input, output) {
    # rv stores the current test state
    rv <- initialiseRV(params)
    # UI is rendered programmatically
    output$ui <- renderUI(rv$current_page@ui)
    # Watch out for the next page
    observeEvent(input$nextPage,
                 nextPage(rv, input))
    # Render a datatable
    output$item_info <- DT::renderDataTable({
      rv$current_page # for some reason changes aren't detected in rv$params$piat$items
      rv$params$piat$items
    },
    server = TRUE,
    options = list(scrollX = TRUE))
    
  }
}

#### Helper functions ####

initialiseRV <- function(params) {
  reactiveValues(test_stack = params$pages[- 1],
                 current_page = params$pages[[1]],
                 params = params)
}

nextPage <- function(rv, input) {
  if (length(rv$test_stack) == 0) {
    stop("No pages left to advance to!")
  } else if (is(rv$test_stack[[1]], "code_block")) {
    current_page <- rv$test_stack[[1]]
    fun <- current_page@fun
    rv$test_stack <- rv$test_stack[- 1]
    do.call(fun, list(rv, input))
    nextPage(rv, input)
  } else if (is(rv$test_stack[[1]], "page")) {
    # Next thing on the stack is a test page
    ## If appropriate, finalise the current page
    msg("Finalising the current page")
    msg("Current page:")
    print(rv$current_page)
    if (is(rv$current_page, "page") &&
        .hasSlot(rv$current_page, "on_complete")) {
      do.call(rv$current_page@on_complete, list(rv, input))
    }
    ## Move to the next page
    rv$current_page <- rv$test_stack[[1]]
    print(rv$current_page)
    rv$test_stack <- rv$test_stack[- 1]
    print(rv$test_stack[1:5])
  } else {
    stop("Don't know how to deal with the next thing on the stack!")
  }
}