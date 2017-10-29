#### Top-level function ####

psychTestServer <- function(params) {
  function(input, output, session) {
    # rv stores the current test state
    rv <- initialiseRV(params)
    # UI is rendered programmatically
    output$ui <- renderUI(tags$div(id = "current_page.ui", rv$current_page@ui))
    # Watch out for the next page
    observeEvent(input$nextPage,
                 nextPage(rv, input))
    # Render outputs
    if (is.null(params$renderOutputs)) NULL else {
      params$renderOutputs(rv = rv, input = input, output = output)
    }
    # Render modals
    if (is.null(params$renderModals)) NULL else {
      params$renderModals(rv = rv, input = input, output = output, session = session)
    }
    # Observe events
    if (is.null(params$observeEvents)) NULL else {
      params$observeEvents(rv = rv, input = input, session = session)
    }
  }
}

#### Helper functions ####

initialiseRV <- function(params) {
  reactiveValues(test_stack = params$pages[- 1],
                 current_page = params$pages[[1]],
                 params = params,
                 admin = FALSE)
}

nextPage <- function(rv, input) {
  if (length(rv$test_stack) == 0) stop("No pages left to advance to!")
  # Check validity of the current page. If validity check fails, quit
  # the current operation.
  if (.hasSlot(rv$current_page, "validate") &&
      !do.call(rv$current_page@validate, list(rv, input))) {
    shinyjs::runjs("document.getElementById('current_page.ui').style.visibility = 'visible'")
    return(FALSE) # i.e. we escape the nextPage evaluation, forcing input revision
  }
  # Perform the <on_complete> function for the current page, if it exists.
  if (.hasSlot(rv$current_page, "on_complete")) {
    do.call(rv$current_page@on_complete, list(rv, input))
  }
  # Deal with the next thing on the stack, whatever it is
  if (is(rv$test_stack[[1]], "code_block")) {
    # Next thing on the stack is a code block.
    # Code blocks are executed immediately, and then 
    # we move to the next page.
    rv$current_page <- rv$test_stack[[1]]
    rv$test_stack <- rv$test_stack[- 1]
    fun <- rv$current_page@fun
    do.call(fun, list(rv, input))
    nextPage(rv, input)
  } else if (is(rv$test_stack[[1]], "page")) {
    # Next thing on the stack is a test page
    rv$current_page <- rv$test_stack[[1]]
    rv$test_stack <- rv$test_stack[- 1]
  } else {
    print(rv$test_stack[[1]])
    stop("Don't know how to deal with the next thing on the stack!")
  }
}

#' Pushes <obj> onto the test stack contained within <rv>.
#' <obj> can either be a single object (e.g. one page) or a list
#' of objects (e.g. a list of pages).
pushToTestStack <- function(obj, rv) {
  rv$test_stack <- c(obj, rv$test_stack)
}
