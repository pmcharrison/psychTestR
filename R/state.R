STATE_RDS_FORMAT <- "v1"

STATE <- R6::R6Class(
  "state",
  public = list(
    initialize = function(opt) {
      self$reactive <- shiny::reactiveValues(
        ui_reactive_trigger = NULL,
        admin = FALSE,
        pilot = FALSE
      )
      self$passive[c("results", "time_started", "language")] <- list(
        new_results(),
        Sys.time(),
        opt$languages[1]
      )
    },
    refresh_ui = function() {
      self$reactive$ui_reactive_trigger <- list(Sys.time(),
                                                sample(.Machine$integer.max, 1))
    },
    save_data = function(file) {
      checkmate::qassert(file, "S1")
      saveRDS(list(format = STATE_RDS_FORMAT,
                   passive = self$passive,
                   reactive = shiny::reactiveValuesToList(self$reactive)),
              file)
    },
    update_data = function(passive, reactive) {
      self$update_passive(passive)
      self$update_reactive(reactive)
    },
    update_passive = function(new) {
      self$passive <- new
    },
    update_reactive = function(new) {
      keys <- names(new)
      values <- unname(new)
      for (i in seq_along(new)) {
        key_i <- keys[[i]]
        value_i <- values[[i]]
        self$reactive[[key_i]] <- value_i
      }
    },
    reactive = NULL,
    passive = list(
      elt_index = 1L,
      p_id = NULL,
      globals = list(),
      locals = list(),
      parent_locals = list(),
      results = NULL,
      time_started = as.POSIXct(NA),
      num_restarts = 0L,
      save_id = 1L,
      previous_save_path = NULL,
      language = as.character(NA),
      demo = FALSE,
      error = NULL,
      answer = NULL,
      closed = FALSE,
      allow_session_saving = TRUE,
      url_params = list()
    )
  )
)

is.state <- function(x) {
  is(x, "state")
}

#' Answer
#'
#' Accesses the \code{answer} slot of the participant's \code{state} object.
#' The \code{answer} slot contains the participant's last response.
#' @param state Participant's \code{state} object.
#' @export
answer <- function(state) {
  stopifnot(is(state, "state"))
  state$passive$answer
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
  state$passive$answer <- value
  state
}

# Save ID

# Accesses the \code{save_id} slot of the participant's \code{state} object.
# This ID counts the number of times that data has been saved for this participant.
# @param state Participant's \code{state} object.
save_id <- function(state) {
  stopifnot(is(state, "state"))
  state$passive$save_id
}

`save_id<-` <- function(state, value) {
  stopifnot(is(state, "state"),
            is.integer(value))
  state$passive$save_id <- value
  state
}

previous_save_path <- function(state) {
  stopifnot(is(state, "state"))
  state$passive$previous_save_path
}

`previous_save_path<-` <- function(state, value) {
  stopifnot(is(state, "state"),
            is.scalar.character(value))
  state$passive$previous_save_path <- value
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
  state$passive$error
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
  state$passive$error <- value
  state
}

admin <- function(state) {
  stopifnot(is(state, "state"))
  state$reactive$admin
}

`admin<-` <- function(state, value) {
  stopifnot(is(state, "state"), is.scalar.logical(value))
  state$reactive$admin <- value
  state
}

pilot <- function(state) {
  stopifnot(is(state, "state"))
  state$reactive$pilot
}

`pilot<-` <- function(state, value) {
  stopifnot(is(state, "state"), is.scalar.logical(value))
  state$reactive$pilot <- value
  state
}

#' Demo
#'
#' Determines whether the current session is a demo session.
#' @param state The participant's \code{state} object.
#' @return \code{TRUE} if the current session is a demo session,
#' \code{FALSE} otherwise.
#' @export
demo <- function(state) {
  stopifnot(is(state, "state"))
  state$passive$demo
}

`demo<-` <- function(state, value) {
  stopifnot(is(state, "state"), is.scalar.logical(value))
  state$passive$demo <- value
  state
}

#' Get URL parameters
#'
#' Gets the current URL parameters as stored in \code{state}.
#' @param state The participant's \code{state} object.
#' @export
get_url_params <- function(state) {
  stopifnot(is(state, "state"))
  state$passive$url_params
}

`url_params<-` <- function(state, value) {
  stopifnot(is(state, "state"), is.list(value))
  state$passive$url_params <- value
  state
}

allow_session_saving <- function(state) {
  stopifnot(is(state, "state"))
  state$passive$allow_session_saving
}

`allow_session_saving<-` <- function(state, value) {
  stopifnot(is(state, "state"), is.scalar.logical(value))
  state$passive$allow_session_saving <- value
  state
}

closed <- function(state) {
  stopifnot(is(state, "state"))
  state$passive$closed
}

`closed<-` <- function(state, value) {
  stopifnot(is(state, "state"), is.scalar.logical(value))
  state$passive$closed <- value
  state
}

language <- function(state) {
  stopifnot(is(state, "state"))
  state$passive$language
}

`language<-` <- function(state, value) {
  stopifnot(is(state, "state"), is.scalar.character(value))
  state$passive$language <- value
  state
}

#' Get session info
#'
#' Gets information about the current participant's session,
#' including things like participant ID, time started, and number of restarts.
#' @param state The participant's \code{state} object.
#' @param complete Whether or not the session is now considered complete
#' (this only affects the returned session info).
#' @return List containing session information.
#' @export
get_session_info <- function(state, complete) {
  stopifnot(is(state, "state"),
            is.scalar.logical(complete))
  res <- list(
    p_id = state$passive$p_id,
    pilot = pilot(state),
    complete = complete,
    time_started = state$passive$time_started,
    current_time = Sys.time(),
    num_restarts = state$passive$num_restarts,
    language = state$passive$language
  )
  attr(res, "server") <- utils::sessionInfo()
  res
}

increment_num_restarts <- function(state) {
  state$passive$num_restarts <- state$passive$num_restarts + 1L
}

# as.list.state <- function(x, ...) {
#   shiny::reactiveValuesToList(x)
# }

#' Get results
#'
#' Gets the participants current results, optionally
#' along with session information.
#' @param state The participant's \code{state} object.
#' @param complete Whether the participant has now completed the test (Boolean).
#' @param add_session_info Whether to add session information (Boolean).
#' @return A \code{results} object.
#' @export
get_results <- function(state, complete, add_session_info = FALSE) {
  stopifnot(is(state, "state"))
  stopifnot(is.scalar.logical(complete))
  results <- state$passive$results
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
  state$passive$globals[[key]]
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
  state$passive$globals[[key]] <- value
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
  state$passive$locals[[key]]
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
  state$passive$locals[[key]] <- value
}

enter_local_environment <- function(state) {
  stopifnot(is(state, "state"))
  ind <- length(state$passive$parent_locals) + 1L
  state$passive$parent_locals[[ind]] <- state$passive$locals
  state$passive$locals <- list()
}

leave_local_environment <- function(state) {
  stopifnot(is(state, "state"))
  ind <- length(state$passive$parent_locals)
  state$passive$locals <- state$passive$parent_locals[[ind]]
}

#' Get participant ID
#'
#' Returns the participant's ID.
#' @param state The participant's \code{state} object.
#' @export
p_id <- function(state) {
  stopifnot(is(state, "state"))
  state$passive$p_id
}

#' Sets participant ID
#'
#' Sets the participant's ID.
#' @param state The participant's \code{state} object.
#' @param value The value to set the ID to.
#' @export
`p_id<-` <- function(state, value) {
  stopifnot(is(state, "state"))
  state$passive$p_id <- value
  return(state)
}

advance_to_first_page <- function(state, input, output, elts, session, opt) {
  stopifnot(is(state, "state"))
  current_elt <- get_current_elt(state, elts = elts, opt = opt, eval = FALSE)
  if (!(is(current_elt, "page") ||
        is(current_elt, "reactive_page")))
    next_page(
      state = state, input = input, output = output,
      elts = elts, session = session, opt = opt
    )
}

get_num_elts <- function(elts) {
  elts$length
}

# get_current_elt_index <- function(state) {
#   stopifnot(is(state, "state"))
#   state$passive$elt_index
# }

get_elt <- function(state, index, elts, opt, eval = TRUE) {
  stopifnot(is.scalar.numeric(index), round(index) == index,
            index >= 0, index <= get_num_elts(elts))
  elt <- elts$get(language(state), index)
  if (is(elt, "reactive_page") && eval) {
    I18N_STATE$set(dict = elt@i18n_dict, lang = language(state))
    res <- elt@fun(state = state, answer = answer(state), opt = opt)
    I18N_STATE$reset()
    res
  } else elt
}

get_current_elt <- function(state, elts, opt, eval = TRUE) {
  get_elt(state = state, index = state$passive$elt_index,
          elts = elts, opt = opt, eval = eval)
}

get_next_elt <- function(state, elts, opt, eval = TRUE) {
  get_elt(state, index = state$passive$elt_index + 1L,
          elts = elts, opt = opt, eval = eval)
}

# Low-level setter, see skip_n_pages for skipping pages in general.
# Note: we allow elt_index to temporarily exceed the span
# of the available elements, as long as this is resolved
# once get_elt is called.
increment_elt_index <- function(state, by = 1L) {
  stopifnot(is.scalar.numeric(by), is.scalar(by), round(by) == by)
  new_index <- state$passive$elt_index + by
  state$passive$elt_index <- new_index
}
