#' export
save_data <- function(state, value, context = NULL, options = get_save_options()) {
  stopifnot(is(options, "save_options"))
  if (!is.null(options$global_key)) set_global(options$global_key, value, state)
  if (options$save_result) {
    key <- if (is.null(options$result_key)) context else options$result_key
    save_result(place = state, key = key, value = value)
  }
}

#' @export
get_save_options <- function(result_key = NULL,
                             save_result = TRUE,
                             global_key = NULL) {
  stopifnot(is.scalar.logical(save_result),
            is.null(global_key) || is.scalar.character(global_key))
  x <- list(result_key = result_key,
            save_result = save_result,
            global_key = global_key)
  class(x) <- "save_options"
  x
}

initialise_results <- function() {
  x <- list()
  class(x) <- "results"
  x
}

#' @export
print.results <- function(x, ...) {
  num_sections <- length(x)
  num_results <- sum(vapply(x, length, integer(1)))
  cat(sprintf("psychTest results list (%i result%s in %s section%s)\n",
              num_results, if (num_results != 1L) "s" else "",
              num_sections, if (num_sections != 1L) "s" else ""))
}

#' @export
as.list.results <- function(x, ...) {
  class(x) <- "list"
  attr(x, "new_section") <- NULL
  x
}

#' @export
new_results_section <- function(x, label) {
  stopifnot(is.null(label) || is.scalar.character(label))
  UseMethod("new_results_section")
}

#' @export
new_results_section.state <- function(x, label) {
  results(x) <- new_results_section(results(x), label)
  NULL
}

#' @export
new_results_section.results <- function(x, label) {
  attr(x, "new_section") <- label
  x
}

#' @export
save_result <- function(place, key, value) UseMethod("save_result")

save_result.results <- function(place, key, value) {
  num_sections <- length(place)
  new_section <- num_sections == 0L || !is.null(attr(place, "new_section"))
  index_1 <- if (new_section) num_sections + 1L else num_sections
  index_2 <- if (new_section) 1L else length(place[[index_1]]) + 1L
  if (new_section) {
    new_section_label <- attr(place, "new_section")
    place[[index_1]] <- list()
    if (!is.null(new_section_label)) names(place)[index_1] <- new_section_label
    attr(place, "new_section") <- NULL
  }
  place[[index_1]][[index_2]] <- list(key = key, value = value)
  place
}

#' @export
save_result.state <- function(place, key, value) {
  place$results <- save_result.results(place$results, key, value)
  place
}

#' @export
results <- function(state) {
  state$results
}

#' @export
`results<-` <- function(state, value) {
  state$results <- value
}
