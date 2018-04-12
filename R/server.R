server <- function(elts, side_panel, options) {
  check_elts(elts)
  function(input, output, session) {
    state <- initialise_state()
    advance_to_first_page(stage, elts)
    output$ui <- render_ui(state)
    observeEvent(input$next_page, next_page(state, input, elts))
    side_panel_server(side_panel, state, input, output, session)
  }
}

next_page <- function(state, input, elts) {
  elt  <- get_current_elt(state, elts, eval = TRUE)
  if (is(elt, "page")) {
    if (!validate_elt(elt, state, input)) {
      return(make_current_page_visible())
    }
    perform_on_complete_function(elt, state, input)
  } else if (is(elt, "code_block")) {
    elt@fun(state)
  }
  increment_elt_index(state, elts)
  new_elt <- get_current_elt(state, elts, eval = FALSE)
  if (is(new_elt, "code_block")) return(next_page(state, input, elts))
}

check_elts <- function(elts) {
  last_elt <- elts[[length(elts)]]
  if (!is(last_elt, "page")) {
    stop("The last element in <elts> must be a test page.")
  }
}

render_ui <- function(state, elts) {
  renderUI({
    elt <- get_current_elt(state, elts)
    if (!is(elt, "page")) error("Cannot display the current test element.")
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

execute_code_block(elt, state) {
  stopifnot(is(elt, "code_block"))
  elt@fun(state)
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
