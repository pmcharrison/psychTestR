# Classes ####

new_results <- function() {
  x <- list()
  class(x) <- "results"
  x <- register_next_results_section(x, "results")
  # attr(x, "metadata") <- list()
  x
}

#' @export
print.results <- function(x, ...) {
  num_sections <- length(x)
  num_results <- sum(vapply(x, length, integer(1)))
  cat(sprintf("psychTestR results list (%i result%s in %s section%s)\n",
              num_results, if (num_results != 1L) "s" else "",
              num_sections, if (num_sections != 1L) "s" else ""),
      "(visualise with as.list() or as.data.frame())\n")
}

#' @export
as.list.results <- function(x, ...) {
  class(x) <- "list"
  attr(x, "new_section") <- NULL
  x
}

#' @export
as_tibble.results <- function(x, ...) {
  y <- unlist(as.list(x), recursive = FALSE)
  y <- purrr::map(y,
                  ~ if (is.atomic(.) && length(.) > 1 && !is.null(names(.)))
                    as.list(.) else .)
  y <- purrr::map(y, ~ if (is.null(names(.)) && length(.) > 1) list(.) else .)
  df <- tibble::as_tibble(y, .name_repair = "minimal")
  stopifnot(nrow(df) == 1)
  session_info_cols <- grepl("^session\\.", names(df))
  df[, c(which(session_info_cols), which(!session_info_cols))]
}

#' @export
as.data.frame.results <- function(x, ...) {
  as.data.frame(tibble::as_tibble(x))
}

# sections <- as.list(x)
# section_labels <- names(x)
# if (is.null(section_labels)) section_labels <- rep(as.character(NA),
#                                                    times = length(sections))
# section_dfs <- mapply(convert_section_to_df, sections, section_labels,
#                       SIMPLIFY = FALSE)
# do.call(plyr::rbind.fill, section_dfs)

# convert_section_to_df <- function(section, section_label) {
#   stopifnot(is.list(section), is.scalar.character(section_label))
#   dfs <- lapply(section, convert_result_to_df)
#   res <- do.call(plyr::rbind.fill, dfs)
#   res <- res[, order(names(res))]
#   res <- cbind(section = section_label, res)
#   res
# }
#
# convert_result_to_df <- function(result) {
#   if (is.list(result)) {
#     elt_lengths <- vapply(result, length, integer(1))
#     for (i in seq_along(elt_lengths)) {
#       elt_length <- elt_lengths[[i]]
#       if (elt_length == 0L) result[[i]] <- NA
#       if (elt_length > 1L) result[[i]] <- list(result[[i]])
#     }
#   } else {
#     result <- list(result = result)
#   }
#   as.data.frame(lapply(result, I))
# }

# Accessing results ####

#' Access results
#'
#' Accesses the \code{results} slot in \code{state}.
#' @param state The participant's \code{state} object.
#' @export
results <- function(state) {
  state$passive$results
}

#' Set results
#'
#' Sets the \code{results} slot in \code{state}.
#' @param state The participant's \code{state} object.
#' @param value Value to set the \code{results} slot to.
#' @export
`results<-` <- function(state, value) {
  state$passive$results <- value
}

# Register next results section ####

#' Register next results section
#' @param x Where should the new section be registered?
#' Methods exist for \code{results} objects and for \code{state} objects.
#' @param label Label for the new section.
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

#' Save result
#'
#' Saves a new datum into the participant's results store.
#'
#' @param place Place to save the results.
#' Methods exist for \code{results} objects and for \code{state} objects.
#' @param label Label for the new datum.
#' @param value Value of the new datum.
#' @export
save_result <- function(place, label, value) UseMethod("save_result")

save_result.results <- function(place, label, value) {
  stopifnot(is.scalar.character(label))
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
  names(place[[index_1]])[[index_2]] <- label
  place
}

#' @export
save_result.state <- function(place, label, value) {
  stopifnot(is.scalar.character(label))
  place$passive$results <- save_result.results(place$passive$results, label, value)
  place
}
