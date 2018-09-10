#' @export
get_supplementary_results_dir <- function(opt) {
  x <- opt$supplementary_results_dir
  if (is.null(x)) stop("supplementary_results_dir missing from options list")
  if (!is.scalar.character(x)) stop("wrong format for supplementary_results_dir")
  R.utils::mkdirs(x)
  x
}
