#' Randomise at run time
#'
#' This helper function constructs a timeline where the order of
#' test elements (or blocks of test elements) is determined at run time.
#'
#' @inheritParams order_at_run_time
#'
#' @return
#' A list of test elements or equivalently a timeline that can be combined
#' with other test elements or timelines.
#' These test elements will be presented in a freshly randomised
#' order for each participant.
#'
#' @export
randomise_at_run_time <- function(
  label,
  logic,
  save_order = function(order, state, ...) save_result(state, label, order)
) {
  checkmate::qassert(label, "S1")
  stopifnot(is.list(logic))
  n <- length(logic)

  order_at_run_time(
    label = label,
    get_order = function(...) sample(n, n, replace = FALSE),
    logic = logic,
    save_order = save_order
  )
}
