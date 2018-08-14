is.scalar.character <- function(x) {
  is.character(x) && is.scalar(x)
}

is.scalar.numeric <- function(x) {
  is.numeric(x) && is.scalar(x)
}

is.scalar.logical <- function(x) {
  is.logical(x) && is.scalar(x)
}

is.scalar <- function(x) {
  identical(length(x), 1L)
}

is.integerlike <- function(x) {
  all(round(x) == x)
}

is.scalar.integerlike <- function(x) {
  is.scalar(x) && is.integerlike(x)
}

tagify <- function(x) {
  stopifnot(is.character(x) || is(x, "shiny.tag"))
  if (is.character(x)) {
    stopifnot(is.scalar(x))
    shiny::p(x)
  } else x
}

is.null.or <- function(x, f) {
  is.null(x) || f(x)
}

#' Display a popup error message
#'
#' Displays a popup error message and terminates the testing session.
#' @param ... Arguments to be pasted together (without separator) to form
#' the error message.
#' @export
display_error <- function(...) {
  msg <- paste(..., collapse = "")
  shinyjs::alert(msg)
  stop(msg)
}

#' Assert that a global variable is NULL
#'
#' Throws an error if a certain global variable is not \code{NULL}.
#' @param key Key of the global variable that should be \code{NULL}.
#' @param state The participant's \code{state} object.
#' @export
assert_global_is_null <- function(key, state) {
  stopifnot(is.scalar.character(key), is(state, "state"))
  if (is.null(get_global(key, state))) TRUE else {
    stop("global variable <", key, "> in <state> was not NULL")
  }
}
