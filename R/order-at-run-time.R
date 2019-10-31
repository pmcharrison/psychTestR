#' @export
order_at_run_time <- function(
  label,
  get_order,
  logic,
  save_order = function(order, state) save_result(state, label, order)
) {
  init_block_queue <- code_block(function(state, ...) {
    order <- get_order(state = state, ...)
    set_global(".block_queue", order, state, allow_dots = TRUE)
    save_order(order = order,
               state = state)
  })

  is_next_block <- function(i) function(state, ...) {
    queue <- get_global(".block_queue", state)
    checkmate::qassert(i, "X1")
    stopifnot(length(queue) > 0)
    queue[1] == i
  }

  any_more_blocks <- function(state, ...) length(get_global(".block_queue", state)) > 0

  clear_block_queue <- code_block(function(state, ...)
    set_global(".block_queue", NULL, state, allow_dots = TRUE))

  pop_block <- code_block(function(state, ...) {
    queue <- get_global(".block_queue", state)
    stopifnot(is.numeric(queue), length(queue) > 0)
    set_global(".block_queue", queue[-1], state, allow_dots = TRUE)
  })

  conditional_blocks <- purrr::map2(logic, seq_along(logic), function(block, i) {
    conditional(is_next_block(i), block)
  })

  loop <- while_loop(logic = c(do.call(c, conditional_blocks), pop_block),
                     test = any_more_blocks)

  c(init_block_queue, loop, clear_block_queue)
}
