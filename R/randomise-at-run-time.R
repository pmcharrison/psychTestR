#' Randomise at run time
#'
#' This helper function constructs a timeline where the order of
#' test elements (or blocks of test elements) is determined at run time.
#'
#' @note This function can be nested arbitrarily many times.
#'
#' @inheritParams order_at_run_time
#'
#' @return
#' A list of test elements or equivalently a timeline that can be combined
#' with other test elements or timelines.
#' These test elements will be presented in a freshly randomised
#' order for each participant.
#'
#' @examples
#' \donttest{
#' join(
#'   randomise_at_run_time(
#'     "main",
#'     list(
#'       randomise_at_run_time(
#'         "1",
#'         list(one_button_page("1a"),
#'              one_button_page("1b"),
#'              one_button_page("1c"))
#'       ),
#'       randomise_at_run_time(
#'         "2",
#'         list(one_button_page("2a"),
#'              one_button_page("2b"),
#'              one_button_page("2c"))
#'       ),
#'       randomise_at_run_time(
#'         "3",
#'         list(one_button_page("3a"),
#'              one_button_page("3b"),
#'              one_button_page("3c"))
#'       )
#'     )
#'   )
#' ) %>% join(final_page("End")) %>% make_test()
#' }
#'
#' @export
randomise_at_run_time <- function(
  label,
  logic,
  save_order = function(order, state, ...) save_result(state, label, order)
) {
  n <- length(logic)

  order_at_run_time(
    label = label,
    get_order = function(...) sample(n, n, replace = FALSE),
    logic = logic,
    save_order = save_order
  )
}
