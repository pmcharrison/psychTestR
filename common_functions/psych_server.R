#### Top-level function ####
files <- list(session_dir = "/var/tmp/piat")
options <- list(session_timeout_min = 120,
                clean_sessions_interval_min = 15)

psychTestServer <- function(params) {
  function(input, output, session) {
    # rv stores the current test state
    rv <- initialiseRV(params)
    # The call to manageSession starts a new session, but if we are continuing a
    # previous session it first calls restore() with the saved data.
    manageSession(
      save = reactive({
        reactiveValuesToList(rv)
      }),
      restore = function(data) {
        if (!is.null(data)) {
          for (i in seq_along(data)) {
            rv[[names(data)[i]]] <- data[[i]]
          }
          rv$resumed <- TRUE
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
      if (!rv$resumed) {
        message("Running setup command")
        params$setup %>% (function(x) if (!is.null(x)) do.call(x, list(rv)))
      }
    }, once = TRUE)
    # # Go to first page (once at beginning of test)
    # observeEvent(TRUE, {
    #   if (!rv$resumed) {
    #     message("Advancing to the first page")
    #     nextPage(rv, input, params)
    #   }
    # }, once = TRUE)
    # UI is rendered programmatically
    output$ui <- renderUI({
      params$pages[[rv$page_index]] %>%
        (function(elt) {
          if (is(elt, "page")) {
            elt
          } else if (is(elt, "reactive_test_element")) {
            do.call(elt@fun, list(rv))
          } else stop ("Unrecognised element type")
        }) %>%
        (function(page) {
          tags$div(id = "current_page.ui",
                   page@ui)
        })
    })
    # Watch out for the next page
    observeEvent(input$nextPage,
                 nextPage(rv, input, params))
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
      params$observeEvents(rv = rv, input = input, session = session, params = params)
    }
  }
}

#### Helper functions ####

listToReactiveValues <- function(l) {
  rv <- reactiveValues()
  for (i in seq_along(l)) {
    rv[[names(l)[i]]] <- l[[i]]
  }
  rv
}

initialiseRV <- function(params) {
  message("Initialising RV")
  reactiveValues(page_index = 1,
                 message = "No message to display",
                 resumed = FALSE)
}

getElement <- function(rv, params, index) {
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
    do.call(elt@fun, list(rv))
  } else elt
}

getCurrentElement <- function(rv, params) {
  current_index <- rv$page_index
  getElement(rv, params, current_index)
}

getNextElement <- function(rv, params) {
  current_index <- rv$page_index
  getElement(rv, params, current_index + 1)
}

incrementPageIndex <- function(rv, by = 1) {
  assertthat::assert_that(
    is.numeric(by),
    assertthat::is.scalar(by),
    round(by) == by
  )
  rv$page_index <- rv$page_index + by
}

decrementPageIndex <- function(rv, by = 1) {
  incrementPageIndex(rv, by = - by)
}

setMessage <- function(rv, message) {
  rv$message <- message
}

# rv can be a reactive values object or a list
nextPage <- function(rv, input, params) {
  # Check validity of the current page. If validity check fails, quit
  # the current operation.
  current_elt  <- getCurrentElement(rv, params)
  message("Running nextPage, with this as the current test element:")
  print(current_elt)
  if (.hasSlot(current_elt, "validate") &&
      !do.call(current_elt@validate, list(rv, input))) {
    shinyjs::runjs("document.getElementById('current_page.ui').style.visibility = 'visible'")
    return(FALSE) # i.e. we escape the nextPage evaluation, forcing input revision
  }
  # Perform the <on_complete> function for the current page, if it exists.
  if (.hasSlot(current_elt, "on_complete")) {
    do.call(current_elt@on_complete, list(rv, input))
  }
  # Deal with the next thing on the stack, whatever it is
  if (is(getNextElement(rv, params), "code_block")) {
    # Next thing on the stack is a code block.
    # Code blocks are executed immediately, and then 
    # we move to the next page.
    incrementPageIndex(rv)
    fun <- getCurrentElement(rv, params)@fun
    do.call(fun, list(rv, input))
    nextPage(rv, input, params)
  } else if (is(getNextElement(rv, params), "page") ||
             is(getNextElement(rv, params), "reactive_test_element")) {
    # Next thing on the stack is a test page
    rv$dep <- Sys.time()
    incrementPageIndex(rv)
  } else {
    stop("Don't know how to deal with the next thing on the stack!")
  }
}
