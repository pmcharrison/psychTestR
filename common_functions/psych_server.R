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
    # Render outputs
    params$renderOutputs(rv, output)
  }
}

#### Helper functions ####

initialiseRV <- function(params) {
  reactiveValues(test_stack = params$pages[- 1],
                 current_page = params$pages[[1]],
                 params = params)
}

nextPage <- function(rv, input) {
  if (length(rv$test_stack) == 0) stop("No pages left to advance to!")
  # Check validity of the current page, if appropriate
  if (is(rv$current_page, "page") &&
      .hasSlot(rv$current_page, "validate") &&
      !do.call(rv$current_page@validate, list(rv, input))) {
    return(FALSE) # i.e. we escape the nextPage evaluation, forcing input revision
  }
  # If appropriate, finalise the current page
  if (is(rv$current_page, "page") &&
      .hasSlot(rv$current_page, "on_complete")) {
    do.call(rv$current_page@on_complete, list(rv, input))
  }
  # Deal with the next thing on the stack, whatever it is
  if (is(rv$test_stack[[1]], "code_block")) {
    rv$current_page <- rv$test_stack[[1]]
    print(rv$current_page)
    fun <- rv$current_page@fun
    rv$test_stack <- rv$test_stack[- 1]
    do.call(fun, list(rv, input))
    nextPage(rv, input)
  } else if (is(rv$test_stack[[1]], "page")) {
    # Next thing on the stack is a test page
    ## Move to the next page
    rv$current_page <- rv$test_stack[[1]]
    rv$test_stack <- rv$test_stack[- 1]
  } else {
    stop("Don't know how to deal with the next thing on the stack!")
  }
}

#' Pushes <obj> onto the test stack contained within <rv>.
#' <obj> can either be a single object (e.g. one page) or a list
#' of objects (e.g. a list of pages).
pushToTextStack <- function(obj, rv) {
  rv$test_stack <- c(obj, rv$test_stack)
}