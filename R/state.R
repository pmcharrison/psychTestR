initialise_state <- function() {
  x <- shiny::reactiveValues(elt_index = 1L,
                             results = initialise_results(),
                             setup_complete = FALSE)
  class(x) <- c(class(x), "state")
  x
}

setup_complete <- function(state) {
  stopifnot(is(state, "state"))
  state$setup_complete
}

`setup_complete<-` <- function(state, value) {
  stopifnot(is(state, "state"), is.scalar.logical(value))
  state$setup_complete <- value
}

advance_to_first_page <- function(state, elts) {
  stopifnot(is(state, "state"))
  current_elt <- get_current_elt(state, elts = elts, eval = FALSE)
  if (!is(current_elt, "page")) next_page(state)
}

get_num_elts <- function(elts) {
  length(elts)
}

get_current_elt_index <- function(state) {
  stopifnot(is(state, "state"))
  state$elt_index
}

get_elt <- function(state, index, elts, eval = TRUE) {
  stopifnot(is.scalar.numeric(index), round(index) == index,
            index >= 0, index <= get_num_elts(elts))
  elt <- elts[[index]]
  if (is(elt, "reactive_test_element") && eval) {
    elt@fun(state)
  } else elt
}

get_current_elt <- function(state, elts, eval = TRUE) {
  current_index <- get_current_elt_index(state)
  get_elt(state = state, index = current_index,
          elts = elts, eval = eval)
}

get_next_elt <- function(state, elts, eval = TRUE) {
  current_index <- get_current_elt_index(state)
  get_elt(state, index = current_index + 1L,
          elts = elts, eval = eval)
}

increment_elt_index <- function(state, elts, by = 1L) {
  stopifnot(is.scalar.numeric(by), is.scalar(by), round(by) == by)
  new_index <- state$elt_index + by
  if (new_index > get_num_elts(elts)) {
    error("Tried to advance past the end of the test.")
  }
  if (new_index < 1L) {
    error("Test indices less than 1 are not permitted.")
  }
  state$elt_index <- new_index
}

decrement_index <- function(state, elts, by = 1) {
  increment_elt_index(state, elts, by = - by)
}
