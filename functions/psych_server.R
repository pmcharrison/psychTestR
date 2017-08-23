#### Top-level function ####

psychTestServer <- function(params) {
  function(input, output) {
    # rv stores the current test state
    rv <- initialiseRV(params)
    # UI is rendered programmatically
    output$ui <- renderUI(rv$current_page@ui)
    # Triggers are monitored to cue next page
    monitorTriggers(rv, input)
  }
}

#### Helper functions ####

initialiseRV <- function(params) {
  reactiveValues(test_stack = params$pages[- 1],
                 current_page = params$pages[[1]],
                 trigger_values = NULL)
}

monitorTriggers <- function(rv, input) {
  observe({
    prev_trigger_values <- rv$trigger_values
    new_trigger_values <- getTriggerValues(rv, input)
    rv$trigger_values <- new_trigger_values
    if (checkTriggers(new_trigger_values, prev_trigger_values)) {
      nextPage(rv, input)
    }
  })
}

getTriggerValues <- function(rv, input) {
  triggers <- rv$current_page@triggers
  trigger_values <- lapply(triggers,
                           function(x) input[[x]])
  trigger_values
}

#' Compares a new set of trigger values to the previous set of 
#' trigger values and works out whether or not the next page 
#' should be triggered, returning a Boolean. Currently written
#' with actionButtons in mind, but should be extensible to
#' other types of input as well as other types of validation.
#' This function has to deal with the somewhat annoying behaviour
#' of actionButtons: when initialised they have a value of NULL
#' then of 0, and when clicked their value increases by 1, but when
#' the next page is cued the old value is kept for a while before
#' (potentially?) returning to 0.
checkTriggers <- function(new_trigger_values, prev_trigger_values) {
  triggering_next_page <- FALSE
  if (length(new_trigger_values) > 0 &&
      length(new_trigger_values) == length(prev_trigger_values)) {
    triggered <- mapply(function(current, initial) {
      !is.null(current) && current > 0 &&
        (is.null(initial) || current > initial)
    }, new_trigger_values, prev_trigger_values)
    triggering_next_page <- any(triggered)
  }
  triggering_next_page
}

nextPage <- function(rv, input) {
  if (length(rv$test_stack) == 0) {
    stop("No pages left to advance to!")
  }
  if (is(rv$test_stack[[1]], "page")) {
    # Next thing on the stack is a test page
    ## Finalise the current page (to do)
    ## Move to the next page
    rv$current_page <- rv$test_stack[[1]]
    rv$test_stack <- rv$test_stack[- 1]
    rv$current_page_initial_trigger_values <- getTriggerValues(rv, input)
  } else {
    stop("Don't know how to deal with the next thing on the stack!")
  }
}