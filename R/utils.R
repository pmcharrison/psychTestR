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

#' Is NULL or...?
#'
#' Returns \code{TRUE} if \code{x} is either \code{NULL} or \code{f(x)}
#' is \code{TRUE}.
#'
#' @param x Object to check.
#'
#' @param f Function to apply.
#'
#' @keywords internal
#' @export
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

#' @rdname global_local
#' @export
assert_global_is_null <- function(key, state) {
  stopifnot(is.scalar.character(key), is(state, "state"))
  if (is.null(get_global(key, state))) TRUE else {
    stop("global variable `", key, "` in `state` was not `NULL`")
  }
}


tidy_user_info <- function(user_info) {

  user_info <- purrr::map(user_info, function(x) {
    if(length(x) > 1) jsonlite::toJSON(x) else x
  })

  user_info <- unlist(user_info) %>%
    as.list() %>% # This looks counterintuitive, but we use unlist for its recursive property, then it's easier to go from a named list to a tibble
    tibble::as_tibble(.name_repair = 'universal')

  # Tidy names for language specifically
  idx <- which(grepl("language", names(user_info)))
  names(user_info)[idx] <- paste0("language", seq_along(idx))

  return(user_info)
}
