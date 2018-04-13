#' export
save_data <- function(state, value, context = NULL, options = save_options()) {
  stopifnot(is(options, "save_options"))
  if (!is.null(options$global_key)) set_global(options$global_key, value, state)
  if (options$save_result) {
    key <- if (is.null(options$result_key)) context else options$result_key
    save_result(place = state, key = key, value = value)
  }
}

#' @export
save_options <- function(result_key = NULL,
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
  x <- list(list())
  class(x) <- "results"
  x <- start_new_results_section(x, "Results")
  print(x)
}

#' @export
start_new_results_section <- function(results, label) {
  stopifnot(is(results, "results"), is.scalar.character(label))
  index <- length(results)
  section <- results[[index]]
  # Overwrite previous section if it was zero-length
  new_index <- if(length(section) == 0L) index else index + 1L
  results[[new_index]] <- list()
  names(results)[[new_index]] <- label
  results
}

#' #' @export
#' new_result <- function(key, data) {
#'   x <- list(key = key, data = data)
#'   class(x) <- "result"
#'   x
#' }

#' @export
save_result <- function(place, key, value) UseMethod("save_result")

save_result.results <- function(place, key, value) {
  # stopifnot(is(result, "result"))
  print(place)
  index_1 <- length(place)
  index_2 <- length(place[[index_1]]) + 1L
  place[[index_1]][[index_2]] <- list(key = key, value = value)
  print(place)
}

#' @export
save_result.state <- function(place, key, value) {
  place$results <- save_result.results(place$results, key, value)
  place
}
