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
  x$language <- NULL
  x$demo <- FALSE
  x$pilot <- FALSE
  x$error <- NULL
  x$answer <- NULL
  x$closed <- FALSE
  x$allow_session_saving <- TRUE
  x$url_params <- list()
  invisible(TRUE)
}

#' Answer
#'
#' Accesses the \code{answer} slot of the participant's \code{state} object.
#' The \code{answer} slot contains the participant's last response.
#' @param state Participant's \code{state} object.
#' @export
answer <- function(state) {
  stopifnot(is(state, "state"))
  state$answer
}

#' Answer
#'
#' Updates the \code{answer} slot of the participant's \code{state} object.
#' The \code{answer} slot contains the participant's last response.
#' @param state Participant's \code{state} object.
#' @param value New value for \code{answer}.
#' @export
`answer<-` <- function(state, value) {
  stopifnot(is(state, "state"))
  state$answer <- value
  state
}

# Save ID

# Accesses the \code{save_id} slot of the participant's \code{state} object.
# This ID counts the number of times that data has been saved for this participant.
# @param state Participant's \code{state} object.
save_id <- function(state) {
  stopifnot(is(state, "state"))
  state$save_id
}

`save_id<-` <- function(state, value) {
  stopifnot(is(state, "state"),
            is.integer(value))
  state$save_id <- value
  state
}

previous_save_path <- function(state) {
  stopifnot(is(state, "state"))
  state$previous_save_path
}

`previous_save_path<-` <- function(state, value) {
  stopifnot(is(state, "state"),
            is.scalar.character(value))
  state$previous_save_path <- value
  state
}

#' Error
#'
#' Accesses the \code{error} slot of the participant's \code{state} object.
#' This slot takes values of \code{NULL} or a character scalar.
#' When not \code{NULL}, the server will display its value as an error message
#' to the participant.
#' @param state Participant's \code{state} object.
#' @export
error <- function(state) {
  stopifnot(is(state, "state"))
  state$error
}

#' Error
#'
#' Sets the \code{error} slot of the participant's \code{state} object.
#' This slot takes values of \code{NULL} or a character scalar.
#' When not \code{NULL}, the server will display its value as an error message
#' to the participant.
#' @param state Participant's \code{state} object.
#' @param value New value for the error slot.
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

pilot <- function(state) {
  stopifnot(is(state, "state"))
  state$pilot
}

`pilot<-` <- function(state, value) {
  stopifnot(is(state, "state"), is.scalar.logical(value))
  state$pilot <- value
  state
}

demo <- function(state) {
  stopifnot(is(state, "state"))
  state$demo
}

`demo<-` <- function(state, value) {
  stopifnot(is(state, "state"), is.scalar.logical(value))
  state$demo <- value
  state
}

#' Get URL parameters
#'
#' Gets the current URL parameters as stored in \code{state}.
#' @param state The participant's \code{state} object.
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

language <- function(state) {
  stopifnot(is(state, "state"))
  state$language
}

`language<-` <- function(state, value) {
  stopifnot(is(state, "state"), is.scalar.character(value))
  state$language <- value
  state
}

#' Get session info
#'
#' Gets information about the current participant's session,
#' including things like participant ID, time started, and number of restarts.
#' @return List containing session information.
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
  attr(res, "server") <- utils::sessionInfo()
  res
}

increment_num_restarts <- function(state) {
  state$num_restarts <- state$num_restarts + 1L
}

as.list.state <- function(x, ...) {
  shiny::reactiveValuesToList(x)
}

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

#' Get results
#'
#' Gets the participants current results, optionally
#' along with session information.
#' @param state The participant's \code{state} object.
#' @param complete Whether the participant has now completed the test (Boolean).
#' @param add_session_info Whether to add session information (Boolean).
#' @return A \code{results} object.
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

#' Get global variable
#'
#' Gets a global variable from the participant's testing session.
#' @param key The variable's key (character scalar).
#' @param state The participant's \code{state} object.
#' @return The global variable with key equal to \code{key}.
#' @export
get_global <- function(key, state) {
  stopifnot(is.scalar.character(key), is(state, "state"))
  state$globals[[key]]
}

#' Set global variable
#'
#' Sets a global variable for the participant's testing session.
#' @param key The variable's key (character scalar).
#' @param value The value to set the variable to.
#' @param state The participant's \code{state} object.
#' @export
set_global <- function(key, value, state) {
  stopifnot(is.scalar.character(key), is(state, "state"))
  state$globals[[key]] <- value
}

#' Get local variable
#'
#' Gets a local variable from the participant's testing session.
#' @param key The variable's key (character scalar).
#' @param state The participant's \code{state} object.
#' @return The local variable with key equal to \code{key}.
#' @export
get_local <- function(key, state) {
  stopifnot(is.scalar.character(key), is(state, "state"))
  state$locals[[key]]
}

#' Set local variable
#'
#' Sets a local variable for the participant's testing session.
#' @param key The variable's key (character scalar).
#' @param value The value to set the variable to.
#' @param state The participant's \code{state} object.
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

#' Get participant ID
#'
#' Returns the participant's ID.
#' @param state The participant's \code{state} object.
#' @export
p_id <- function(state) {
  stopifnot(is(state, "state"))
  state$p_id
}

#' Sets participant ID
#'
#' Sets the participant's ID.
#' @param state The participant's \code{state} object.
#' @param value The value to set the ID to.
#' @export
`p_id<-` <- function(state, value) {
  stopifnot(is(state, "state"))
  state$p_id <- value
  return(state)
}

advance_to_first_page <- function(state, input, output, elts, session, opt) {
  stopifnot(is(state, "state"))
  current_elt <- get_current_elt(state, elts = elts, opt = opt, eval = FALSE)
  if (!is(current_elt, "page")) next_page(
    state = state, input = input, output = output,
    elts = elts, session = session, opt = opt)
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
    I18N_STATE$set(dict = elt@i18n_dict, lang = language(state))
    elt@fun(state = state, answer = answer(state), opt = opt)
    I18N_STATE$reset()
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
