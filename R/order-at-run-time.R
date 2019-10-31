#' Order at run time
#'
#' This helper function constructs a timeline where the order of
#' test elements (or blocks of test elements) is determined at run time.
#'
#' @note This function can be nested arbitrarily many times.
#'
#' @param label
#' (Character scalar)
#' A label for storing the generated order of test elements.
#'
#' @param get_order
#' Function that should accept the arguments \code{state} and \code{...}.
#' It will be called at run time, being passed the participant's
#' state object. It should return a permutation of the integers 1 to N,
#' where N is the length of the \code{logic} argument.
#'
#' @param logic
#' List where each top-level element corresponds to a randomisation unit.
#' These randomisation units may be either single test elements,
#' lists of test elements, or timelines.
#'
#' @param save_order
#' A function called at run time to save the result of \code{get_order}.
#' It should accept three arguments: \code{order}, \code{state}, and \code{...}.
#' \code{order} corresponds to the output of \code{get_order},
#' whereas \code{state} corresponds to the participant's state object.
#' The default setting saves the generated order in psychTestR's
#' default results repository, under the label \code{label}.
#'
#' @return
#' A list of test elements, or equivalently a timeline, which can be combined
#' with other test elements or timelines.
#' These test elements will be presented in the order defined by the
#' \code{get_order} function, with this function being called
#' afresh for each new participant.
#'
#' @seealso \code{\link{randomise_at_run_time}} for the common application
#' of randomising the order of test elements anew for each participant.
#'
#' @export
order_at_run_time <- function(
  label,
  get_order,
  logic,
  save_order = function(order, state, ...) save_result(state, label, order)
) {
  get_block_queues <- function(state) get_global(".block_queues", state)

  set_block_queues <- function(block_queues, state) {
    set_global(".block_queues", block_queues, state, allow_dots = TRUE)
  }

  add_queue <- function(block_queues, queue) {
    block_queues[[length(block_queues) + 1]] <- queue
    block_queues
  }

  init_block_queue <- code_block(function(state, ...) {
    order <- get_order(state = state, ...)
    stopifnot(is.numeric(order),
              !anyDuplicated(order),
              all(order %in% seq_along(logic)))
    set_block_queues(get_block_queues(state) %>% add_queue(order), state)
    save_order(order = order, state = state, ...)
  })

  get_current_queue <- function(state) {
    block_queues <- get_block_queues(state)
    block_queues[[length(block_queues)]]
  }

  pop_queue <- function(block_queues) {
    stopifnot(length(block_queues) > 0)
    block_queues[- length(block_queues)]
  }

  is_next_block <- function(i) function(state, ...) {
    checkmate::qassert(i, "X1")
    current_queue <- get_current_queue(state)
    stopifnot(length(current_queue) > 0)
    current_queue[1] == i
  }

  any_more_blocks <- function(state, ...) {
    length(get_current_queue(state)) > 0
  }

  clear_block_queue <- code_block(function(state, ...)
    set_block_queues(get_block_queues(state) %>% pop_queue(), state))

  pop_block <- code_block(function(state, ...) {
    queue <- get_current_queue(state)
    stopifnot(is.numeric(queue), length(queue) > 0)
    block_queues <- get_block_queues(state)
    block_queues[[length(block_queues)]] <- queue[-1]
    set_block_queues(block_queues, state)
  })

  conditional_blocks <- purrr::map2(logic, seq_along(logic), function(block, i) {
    conditional(is_next_block(i), block)
  })

  loop <- while_loop(logic = c(do.call(c, conditional_blocks), pop_block),
                     test = any_more_blocks)

  c(init_block_queue, loop, clear_block_queue)
}
