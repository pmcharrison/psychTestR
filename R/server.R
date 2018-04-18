options <- list(session_timeout_min = 120,
                clean_sessions_interval_min = 15)

server <- function(elts, side_panel, options) {
  check_elts(elts)
  stopifnot(is(side_panel, "side_panel"))
  function(input, output, session) {
    state <- initialise_state()
    setup_session(state, elts)
    output$ui <- render_ui(state, elts)
    shiny::observeEvent(input$next_page, next_page(state, input, elts, session))
    side_panel_server(side_panel, state, input, output, session)
    manage_sessions(state, options = options, session = session)
  }
}

# manage_sessions <- function(state) {
#   list(
#     shiny::onBookmark(function(x) {
#       x$values$state <- shiny::reactiveValuesToList(state)
#     }),
#     shiny::onRestore(function(x) {
#       update_state_from_list(state, x$values$state)
#     }),
#     onBookmarked(function(url) {
#       updateQueryString(url)
#     })
#   )
# }

setup_session <- function(state, elts) {
  shiny::observeEvent(TRUE, {
    if (!setup_complete(state)) {
      advance_to_first_page(state, elts)
      setup_complete(state) <- TRUE
    }
  }, once = TRUE)
}

next_page <- function(state, input, elts, session) {
  elt  <- get_current_elt(state, elts, eval = TRUE)
  success <- FALSE
  if (is(elt, "page")) {
    success <- try_finalise_page(elt, state, input)
    if (!success) make_current_page_visible()
  } else if (is(elt, "code_block")) {
    elt@fun(state)
    success <- TRUE
  }
  if (success) {
    increment_elt_index(state, elts)
    new_elt <- get_current_elt(state, elts, eval = FALSE)
    if (is(new_elt, "code_block")) return(next_page(state, input, elts, session))
  }
}

try_finalise_page <- function(elt, state, input) {
  stopifnot(is(elt, "page"), is(state, "state"))
  if (elt@final) {
    shinyjs::alert("Cannot advance on a 'final' page!")
    FALSE
  } else if (!validate_elt(elt, state, input)) {
    message("Input validation failed.")
    FALSE
  } else {
    perform_on_complete_function(elt, state, input)
    TRUE
  }
}

execute_code_block <- function(elt, state) {
  stopifnot(is(elt, "code_block"))
  elt@fun(state)
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
    elt <- get_current_elt(state, elts, eval = TRUE)
    if (!is(elt, "page")) error("Cannot display element of class ", class(elt))
    shiny::div(id = "current_page.ui", elt@ui)
  })
}

validate_elt <- function(elt, state, input) {
  elt@validate(state, input)
}

make_current_page_visible <- function() {
  shinyjs::runjs("document.getElementById('current_page.ui').style.visibility = 'visible'")
}

perform_on_complete_function <- function(elt, state, input) {
  elt@on_complete(state, input)
}

# listToReactiveValues <- function(l) {
#   x <- reactiveValues()
#   for (i in seq_along(l)) {
#     x[[names(l)[i]]] <- l[[i]]
#   }
#   x
# }

# setMessage <- function(state, message) {
#   state$message <- message
# }
