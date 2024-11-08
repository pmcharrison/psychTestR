#' Supplementary results directory
#'
#' Returns the location of the supplementary results directory.
#' The supplementary results directory provides a place to store
#' results in addition to the results saved by the psychTestR
#' function \code{save_results_to_disk}.
#' The researcher has full control over what to put in the supplementary
#' results directory.
#' @note The supplementary results directory is typically stored
#' within the main results directory. It will be deleted if the
#' researcher presses the 'Delete all results' button in the admin panel.
#' @param opt Options list as created by \code{test_options()}.
#' @return Character scalar identifying the (relative)
#' path to the supplementary results directory.
#' @export
get_supplementary_results_dir <- function(opt) {
  x <- opt$supplementary_results_dir
  if (is.null(x)) stop("`supplementary_results_dir` missing from options list")
  if (!is.scalar.character(x)) stop("wrong format for `supplementary_results_dir`")
  R.utils::mkdirs(x)
  x
}
