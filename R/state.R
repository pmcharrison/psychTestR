new_state <- function(input, output, session) {
  x <- shiny::reactiveValues()
  class(x) <- c(class(x), "state")
  initialise_state(x)
  x
}

initialise_state <- function(x) {
  stopifnot(is(x, "state"))
  x$elt_index <- 1L
  x$p_id <- NULL
  x$globals <- list()
  x$results <- new_results()
  x$time_started <- Sys.time()
  x$num_restarts <- 0L
  x$admin <- FALSE
  x$error <- NULL
  x$answer <- NULL
  invisible(TRUE)
}

#' @export
answer <- function(state) {
  stopifnot(is(state, "state"))
  state$answer
}

#' @export
`answer<-` <- function(state, value) {
  stopifnot(is(state, "state"))
  state$answer <- value
  state
}

#' @export
error <- function(state) {
  stopifnot(is(state, "state"))
  state$error
}

#' @export
`error<-` <- function(state, value) {
  stopifnot(is(state, "state"), is.scalar.character(value))
  state$error <- value
  state
}

admin <- function(state) {
  stopifnot(is(state, "state"))
  state$admin
}

`admin<-` <- function(state, value) {
  stopifnot(is(state, "state"), is.scalar.logical(value))
  state$admin <- value
  state
}

admin_enable <- function(state) {
  state$admin <- TRUE
}

admin_disable <- function(state) {
  state$admin <- FALSE
}

#' @export
get_session_info <- function(state) {
  stopifnot(is(state, "state"))
  list(
    p_id = state$p_id,
    time_started = state$time_started,
    current_time = Sys.time(),
    num_restarts = state$num_restarts
  )
}

#' @export
increment_num_restarts <- function(state) {
  state$num_restarts <- state$num_restarts + 1L
}

#' @export
as.list.state <- function(x, ...) {
  shiny::reactiveValuesToList(x)
}

#' @export
update_state_from_list <- function(state, list) {
  stopifnot(is(state, "state"), is.list(list))
  keys <- names(list)
  values <- unname(list)
  for (i in seq_along(list)) {
    key_i <- keys[[i]]
    value_i <- values[[i]]
    state[[key_i]] <- value_i
  }
  invisible(TRUE)
}

#' @export
get_results <- function(state, add_session_info = FALSE) {
  stopifnot(is(state, "state"))
  results <- state$results
  if (add_session_info) {
    results <- register_next_results_section(results, "session_info")
    results <- save_result(results, get_session_info(state))
  }
  results
}

#' @export
get_global <- function(key, state) {
  stopifnot(is.scalar.character(key), is(state, "state"))
  state$globals[[key]]
}

#' @export
set_global <- function(key, value, state) {
  stopifnot(is.scalar.character(key), is(state, "state"))
  state$globals[[key]] <- value
}

#' @export
p_id <- function(state) {
  stopifnot(is(state, "state"))
  state$p_id
}

#' @export
`p_id<-` <- function(state, value) {
  stopifnot(is(state, "state"))
  state$p_id <- value
  return(state)
}

advance_to_first_page <- function(state, input, elts, session) {
  stopifnot(is(state, "state"))
  current_elt <- get_current_elt(state, elts = elts, eval = FALSE)
  if (!is(current_elt, "page")) next_page(state, input, elts, session)
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
  if (is(elt, "reactive_page") && eval) {
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

#' Low-level setter, see skip_n_pages for skipping pages in general
increment_elt_index <- function(state, by = 1L) {
  stopifnot(is.scalar.numeric(by), is.scalar(by), round(by) == by)
  new_index <- state$elt_index + by
  if (new_index < 1L) {
    display_error("Test indices less than 1 are not permitted.")
  }
  state$elt_index <- new_index
}
