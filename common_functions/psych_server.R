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
        rv$params$setup %>% (function(x) if (!is.null(x)) do.call(x, list(rv)))
      }
    }, once = TRUE)
    # Go to first page (once at beginning of test)
    observeEvent(TRUE, {
      if (!rv$resumed) {
        message("Advancing to the first page")
        nextPage(rv, input)
      }
    }, once = TRUE)
    # UI is rendered programmatically
    output$ui <- renderUI({
      tags$div(id = "current_page.ui",
               if (is(rv$current_page, "page")) {
                 rv$current_page@ui
                 } else {
                   tags$div()
                 })
    })
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

listToReactiveValues <- function(l) {
  rv <- reactiveValues()
  for (i in seq_along(l)) {
    rv[[names(l)[i]]] <- l[[i]]
  }
  rv
}

initialiseRV <- function(params) {
  message("Initialising RV")
  reactiveValues(test_stack = params$pages,
                 current_page = tags$div(),
                 # current_page = new("one_btn_page",
                 #                    button_text = "Start"),
                 params = params,
                 resumed = FALSE)
}

# rv can be a reactive values object or a list
nextPage <- function(rv, input) {
  sprintf("admin$num_items = %s", format(rv$admin$num_items))
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
    rv$dep <- Sys.time()
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
