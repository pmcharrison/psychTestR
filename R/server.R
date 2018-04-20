options <- list(session_timeout_min = 120,
                clean_sessions_interval_min = 15)

server <- function(elts, side_panel, options) {
  check_elts(elts)
  stopifnot(is(side_panel, "side_panel"))
  function(input, output, session) {
    state <- new_state()
    setup_session(state, input, elts, session, options)
    output$ui <- render_ui(state, elts)
    shiny::observeEvent(input$next_page,
                        next_page(state, input, elts, session, options))
    side_panel_server(side_panel, state, input, output, session)
    admin_panel.server(state, input, output, session, options)
    manage_sessions(state, options = options, session = session)
  }
}

setup_session <- function(state, input, elts, session, options) {
  shiny::isolate({
    if (is_test_closed()) {
      error(state) <- options$server_closed_msg
      return(NULL)
    }
    max <- options$max_num_participants
    if (!is.null(max)) {
      count <- count_participants(options$results_dir)
      if (count + 1L > max) {
        error(state) <- options$max_participants_msg
       return(NULL)
      }
    }
    advance_to_first_page(state, input, elts, session)
  })
}

# safe.next_page <- function(...) {
#   tryCatch(
#     next_page(...),
#     error = function(e) {
#       error(state) <- "An error occurred when trying to advance to the next page."
#     })}

next_page <- function(state, input, elts, session, options) {
  if (is.null(input$last_btn_pressed)) {
    error(state) <- "An unexpected error occurred."
    return()
  }
  elt  <- get_current_elt(state, elts, eval = TRUE)
  success <- FALSE
  if (is(elt, "page")) {
    success <- try_finalise_page(elt, state, input, session, options)
    if (!success) make_current_page_visible()
  } else if (is(elt, "code_block")) {
    execute_code_block(elt, state, options)
    success <- TRUE
  }
  if (success) {
    increment_elt_index(state, elts)
    new_elt <- get_current_elt(state, elts, eval = FALSE)
    if (is(new_elt, "code_block")) return(next_page(state, input = input,
                                                    elts = elts, session = session,
                                                    options = options))
  }
}

try_finalise_page <- function(elt, state, input, session, options) {
  stopifnot(is(elt, "page"), is(state, "state"))
  if (elt@final) {
    shinyjs::alert("Cannot advance on a 'final' page!")
    FALSE
  } else if (!validate_elt(elt, state, input)) {
    message("Input validation failed.")
    FALSE
  } else {
    perform_on_complete_function(elt, state, input, session, options)
    TRUE
  }
}

execute_code_block <- function(elt, state, options) {
  stopifnot(is(elt, "code_block"))
  elt@fun(state = state, options = options)
}

check_elts <- function(elts) {
  last_elt <- elts[[length(elts)]]
  if (!is(last_elt, "page")) {
    stop("The last element in <elts> must be a test page.")
  }
  if (!last_elt@final) {
    stop("The last element in <elts> must be marked 'final' ",
         "(try setting final = TRUE in the last test page).")
  }
}

render_ui <- function(state, elts) {
  shiny::renderUI({
    elt <- if (!is.null(error(state))) {
      final_page(error(state))
    } else {
      get_current_elt(state, elts, eval = TRUE)
    }
    if (!is(elt, "page")) display_error("Cannot display element of class ", class(elt))
    shiny::div(id = "current_page.ui", elt@ui)
  })
}

validate_elt <- function(elt, state, input) {
  elt@validate(state = state, input = input)
}

make_current_page_visible <- function() {
  shinyjs::runjs("document.getElementById('current_page.ui').style.visibility = 'visible'")
}

perform_on_complete_function <- function(elt, state, input, session, options) {
  elt@on_complete(state = state, input = input, session = session,
                  options = options)
}

#' @export
close_test <- function() {
  closed <- file.exists("closed.txt")
  if (closed) {
    shiny::showNotification("Test is already closed.")
  } else {
    success <- file.create("closed.txt")
    if (success) {
      shiny::showNotification("Test successfully closed.")
    } else {
      shiny::showNotification("Failed to close test.")
    }
  }
}

#' @export
open_test <- function() {
  closed <- file.exists("closed.txt")
  if (!closed) {
    shiny::showNotification("Test is already open.")
  } else {
    success <- file.remove("closed.txt")
    if (success) {
      shiny::showNotification("Test successfully opened.")
    } else {
      shiny::showNotification("Failed to open test.")
    }
  }
}

#' @export
is_test_closed <- function() {
  file.exists("closed.txt")
}
