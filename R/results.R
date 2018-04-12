initialise_results <- function() {
  x <- list()
  class(x) <- "results"
  start_new_results_section(x, "Introduction")
  x
}

#' @export
start_new_results_section <- function(results, label) {
  stopifnot(is(results, "results"), is.scalar.character(label))
  index <- length(x) + 1L
  x[[index]] <- list()
  names(x)[[index]] <- label
  x
}

#' @export
new_result <- function(key, data) {
  x <- list(key = key, data = data)
  class(x) <- "result"
  x
}
#' @export
save_result <- UseMethod("save_result")

save_result.results <- function(place, result) {
  stopifnot(is(result, "result"))
  index_1 <- length(place)
  index_2 <- length(place[[index_1]]) + 1L
  place[[index_1]][[index_2]] <- result
  place
}

#' @export
save_result.state <- function(place, result) {
  place$results <- save_result.results(place$results, result)
  place
}
