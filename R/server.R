initialise_state <- function() {
  message("Initialising state")
  x <- reactiveValues(page_index = 1L,
                      message = "No message to display",
                      results = initialise_results(),
                      resumed = FALSE)
  class(x) <- "state"
  x
}

#### Top-level function ####
files <- list(session_dir = "/var/tmp/piat")
options <- list(session_timeout_min = 120,
                clean_sessions_interval_min = 15)

psychTestServer <- function(params) {
  function(input, output, session) {
    # state stores the current test state
    state <- initialiseRV(params)
    # The call to manageSession starts a new session, but if we are continuing a
    # previous session it first calls restore() with the saved data.
    manageSession(
      save = reactive({
        reactiveValuesToList(state)
      }),
      restore = function(data) {
        if (!is.null(data)) {
          for (i in seq_along(data)) {
            state[[names(data)[i]]] <- data[[i]]
          }
          state$resumed <- TRUE
        }
      },
      files = files
    )
    # Automatically clean the session directory every few minutes
    observe({
      millis <- options$clean_sessions_interval_min * 60 * 1000
      invalidateLater(millis, session)
      cleanSessionDir(session_dir = files$session_dir,
                      timeout_min = options$session_timeout_min)
    })
    # Run setup (once at beginning of test)
    observeEvent(TRUE, {
      if (!state$resumed) {
        message("Running setup command")
        params$setup %>% (function(x) if (!is.null(x)) do.call(x, list(state)))
      }
    }, once = TRUE)
    # # Go to first page (once at beginning of test)
    # observeEvent(TRUE, {
    #   if (!state$resumed) {
    #     message("Advancing to the first page")
    #     nextPage(state, input, params)
    #   }
    # }, once = TRUE)
    # UI is rendered programmatically
    output$ui <- renderUI({
      params$pages[[state$page_index]] %>%
        (function(elt) {
          if (is(elt, "page")) {
            elt
          } else if (is(elt, "reactive_test_element")) {
            do.call(elt@fun, list(state))
          } else stop ("Unrecognised element type")
        }) %>%
        (function(page) {
          tags$div(id = "current_page.ui",
                   page@ui)
        })
    })
    # Watch out for the next page
    observeEvent(input$nextPage,
                 nextPage(state, input, params))
    # Render outputs
    if (is.null(params$renderOutputs)) NULL else {
      params$renderOutputs(state = state, input = input, output = output)
    }
    # Render modals
    if (is.null(params$renderModals)) NULL else {
      params$renderModals(state = state, input = input, output = output, session = session)
    }
    # Observe events
    if (is.null(params$observeEvents)) NULL else {
      params$observeEvents(state = state, input = input, session = session, params = params)
    }
  }
}

#### Helper functions ####

listToReactiveValues <- function(l) {
  x <- reactiveValues()
  for (i in seq_along(l)) {
    x[[names(l)[i]]] <- l[[i]]
  }
  x
}

getElement <- function(state, params, index) {
  num_pages <- length(params$pages)
  message("index = ", index)
  message("num_pages = ", num_pages)
  assertthat::assert_that(
    is.numeric(index),
    assertthat::is.scalar(index),
    round(index) == index,
    index >= 0,
    index <= num_pages
  )
  elt <- params$pages[[index]]
  if (is(elt, "reactive_test_element")) {
    do.call(elt@fun, list(state))
  } else elt
}

getCurrentElement <- function(state, params) {
  current_index <- state$page_index
  getElement(state, params, current_index)
}

getNextElement <- function(state, params) {
  current_index <- state$page_index
  getElement(state, params, current_index + 1)
}

incrementPageIndex <- function(state, by = 1) {
  assertthat::assert_that(
    is.numeric(by),
    assertthat::is.scalar(by),
    round(by) == by
  )
  state$page_index <- state$page_index + by
}

decrementPageIndex <- function(state, by = 1) {
  incrementPageIndex(state, by = - by)
}

setMessage <- function(state, message) {
  state$message <- message
}

# state can be a reactive values object or a list
nextPage <- function(state, input, params) {
  # Check validity of the current page. If validity check fails, quit
  # the current operation.
  current_elt  <- getCurrentElement(state, params)
  message("Running nextPage, with this as the current test element:")
  print(current_elt)
  if (.hasSlot(current_elt, "validate") &&
      !do.call(current_elt@validate, list(state, input))) {
    shinyjs::runjs("document.getElementById('current_page.ui').style.visibility = 'visible'")
    return(FALSE) # i.e. we escape the nextPage evaluation, forcing input revision
  }
  # Perform the <on_complete> function for the current page, if it exists.
  if (.hasSlot(current_elt, "on_complete")) {
    do.call(current_elt@on_complete, list(state, input))
  }
  # Deal with the next thing on the stack, whatever it is
  if (is(getNextElement(state, params), "code_block")) {
    # Next thing on the stack is a code block.
    # Code blocks are executed immediately, and then
    # we move to the next page.
    incrementPageIndex(state)
    fun <- getCurrentElement(state, params)@fun
    do.call(fun, list(state, input))
    nextPage(state, input, params)
  } else if (is(getNextElement(state, params), "page") ||
             is(getNextElement(state, params), "reactive_test_element")) {
    # Next thing on the stack is a test page
    state$dep <- Sys.time()
    incrementPageIndex(state)
  } else {
    stop("Don't know how to deal with the next thing on the stack!")
  }
}
