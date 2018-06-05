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
  x$locals <- list()
  x$parent_locals <- list()
  x$results <- new_results()
  x$time_started <- Sys.time()
  x$num_restarts <- 0L
  x$save_id <- 1L
  x$previous_save_path <- NULL
  x$admin <- FALSE
  x$demo <- FALSE
  x$pilot <- FALSE
  x$error <- NULL
  x$answer <- NULL
  x$closed <- FALSE
  x$allow_session_saving <- TRUE
  x$url_params <- list()
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
save_id <- function(state) {
  stopifnot(is(state, "state"))
  state$save_id
}

#' @export
`save_id<-` <- function(state, value) {
  stopifnot(is(state, "state"),
            is.integer(value))
  state$save_id <- value
  state
}

#' @export
previous_save_path <- function(state) {
  stopifnot(is(state, "state"))
  state$previous_save_path
}

#' @export
`previous_save_path<-` <- function(state, value) {
  stopifnot(is(state, "state"),
            is.scalar.character(value))
  state$previous_save_path <- value
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

#' @export
admin <- function(state) {
  stopifnot(is(state, "state"))
  state$admin
}

`admin<-` <- function(state, value) {
  stopifnot(is(state, "state"), is.scalar.logical(value))
  state$admin <- value
  state
}

pilot <- function(state) {
  stopifnot(is(state, "state"))
  state$pilot
}

`pilot<-` <- function(state, value) {
  stopifnot(is(state, "state"), is.scalar.logical(value))
  state$pilot <- value
  state
}

#' @export
demo <- function(state) {
  stopifnot(is(state, "state"))
  state$demo
}

#' @export
`demo<-` <- function(state, value) {
  stopifnot(is(state, "state"), is.scalar.logical(value))
  state$demo <- value
  state
}

#' @export
get_url_params <- function(state) {
  stopifnot(is(state, "state"))
  state$url_params
}

`url_params<-` <- function(state, value) {
  stopifnot(is(state, "state"), is.list(value))
  state$url_params <- value
  state
}

allow_session_saving <- function(state) {
  stopifnot(is(state, "state"))
  state$allow_session_saving
}

`allow_session_saving<-` <- function(state, value) {
  stopifnot(is(state, "state"), is.scalar.logical(value))
  state$allow_session_saving <- value
  state
}

closed <- function(state) {
  stopifnot(is(state, "state"))
  state$closed
}

`closed<-` <- function(state, value) {
  stopifnot(is(state, "state"), is.scalar.logical(value))
  state$closed <- value
  state
}

#' @export
get_session_info <- function(state, complete) {
  stopifnot(is(state, "state"),
            is.scalar.logical(complete))
  res <- list(
    p_id = state$p_id,
    pilot = state$pilot,
    complete = complete,
    time_started = state$time_started,
    current_time = Sys.time(),
    num_restarts = state$num_restarts
  )
  attr(res, "server") <- sessionInfo()
  res
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
#' @param complete Whether the participant completed the test.
get_results <- function(state, complete, add_session_info = FALSE) {
  stopifnot(is(state, "state"))
  stopifnot(is.scalar.logical(complete))
  results <- state$results
  if (add_session_info) {
    results <- register_next_results_section(results, "session")
    session_info <- get_session_info(state, complete)
    for (i in seq_along(session_info)) {
      results <- save_result(results,
                             label = names(session_info)[[i]],
                             value = session_info[[i]])
    }
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
get_local <- function(key, state) {
  stopifnot(is.scalar.character(key), is(state, "state"))
  state$locals[[key]]
}

#' @export
set_local <- function(key, value, state) {
  stopifnot(is.scalar.character(key), is(state, "state"))
  state$locals[[key]] <- value
}

enter_local_environment <- function(state) {
  stopifnot(is(state, "state"))
  ind <- length(state$parent_locals) + 1L
  state$parent_locals[[ind]] <- state$locals
  state$locals <- list()
}

leave_local_environment <- function(state) {
  stopifnot(is(state, "state"))
  ind <- length(state$parent_locals)
  state$locals <- state$parent_locals[[ind]]
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

advance_to_first_page <- function(state, input, elts, session, opt) {
  stopifnot(is(state, "state"))
  current_elt <- get_current_elt(state, elts = elts, opt = opt, eval = FALSE)
  if (!is(current_elt, "page")) next_page(state, input, elts, session)
}

get_num_elts <- function(elts) {
  length(elts)
}

get_current_elt_index <- function(state) {
  stopifnot(is(state, "state"))
  state$elt_index
}

get_elt <- function(state, index, elts, opt, eval = TRUE) {
  stopifnot(is.scalar.numeric(index), round(index) == index,
            index >= 0, index <= get_num_elts(elts))
  elt <- elts[[index]]
  if (is(elt, "reactive_page") && eval) {
    elt@fun(state = state, answer = answer(state), opt = opt)
  } else elt
}

get_current_elt <- function(state, elts, opt, eval = TRUE) {
  current_index <- get_current_elt_index(state)
  get_elt(state = state, index = current_index,
          elts = elts, opt = opt, eval = eval)
}

get_next_elt <- function(state, elts, opt, eval = TRUE) {
  current_index <- get_current_elt_index(state)
  get_elt(state, index = current_index + 1L,
          elts = elts, opt = opt, eval = eval)
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
