# Classes ####

new_results <- function() {
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

# as.data.frame.results <- function(x, ...) {
#   l <- as.list(x)
#   m <- sapply(l, function(section) {
#     lapply(section, function(entry) {
#       o <- unlist(entry, recursive = FALSE)
#       p <- lapply(o, list)
#       as.data.frame(lapply(p, I))
#     })
#   }, simplify = FALSE)
#   stop()
# }

# Accessing results ####

#' @export
results <- function(state) {
  state$results
}

#' @export
`results<-` <- function(state, value) {
  state$results <- value
}

# Register next results section ####

#' @export
register_next_results_section <- function(x, label) {
  stopifnot(is.null(label) || is.scalar.character(label))
  UseMethod("register_next_results_section")
}

#' @export
register_next_results_section.state <- function(x, label) {
  results(x) <- register_next_results_section(results(x), label)
  NULL
}

#' @export
register_next_results_section.results <- function(x, label) {
  attr(x, "new_section") <- label
  x
}

# Saving results ####

#' @export
save_result <- function(place, value) UseMethod("save_result")

save_result.results <- function(place, value) {
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
  place[[index_1]][[index_2]] <- value
  place
}

#' @export
save_result.state <- function(place, value) {
  place$results <- save_result.results(place$results, value)
  place
}
