#' @export
adapt_test <- function(label,
                       item_bank,
                       show_item,
                       stopping_rule = adapt_test.stop_rule.num_items(n = NULL),
                       opt = adapt_test.options()) {
  adapt_test.check_inputs(label, item_bank, show_item)
  c(
    adapt_test.setup(label, opt),
    loop_while(
      fun = adapt_test.check_stopping_rule(stopping_rule),
      logic = c(
        adapt_test.select_next_item(item_bank, opt),
        adapt_test.administer_next_item(item_bank, show_item),
        adapt_test.save_result(item_bank, opt))),
    adapt_test.finalise(opt))
}

#' @export
adapt_test.options <- function(next_item.criterion = "MFI",
                               next_item.estimator = "BM",
                               next_item.prior_dist = "norm",
                               next_item.prior_par = c(0, 1),
                               final_ability.estimator = "BM",
                               cb_control = NULL,
                               cb_group = NULL) {
  stopifnot(
    is.scalar.character(next_item.criterion),
    is.scalar.character(next_item.estimator),
    is.scalar.character(next_item.prior_dist),
    is.scalar.character(final_ability.estimator),
    is.numeric(next_item.prior_par),
    length(next_item.prior_par) == 2L,
    next_item.estimator %in% c("ML", "BM", "EAP", "WL"),
    final_ability.estimator %in% c("ML", "BM", "EAP", "WL")
  )
  list(
    next_item.criterion = next_item.criterion,
    next_item.estimator = next_item.estimator,
    next_item.prior_dist = next_item.prior_dist,
    next_item.prior_par = next_item.prior_par,
    final_ability.estimator = final_ability.estimator,
    cb_control = cb_control,
    cb_group = cb_group
  )
}

# returns TRUE if we should stop
#' @export
adapt_test.stop_rule.num_items <- function(n) {
  if (is.null(n)) {
    stop("number of items must be provided as an argument to ",
         "<adapt_test.stop_rule.num_items>")
  }
  stopifnot(is.scalar.numeric(n), n > 0)
  function(test_state) {
    adapt_test.get_num_items_administered(test_state) == n
  }
}

#' @export
adapt_test.get_num_items_administered <- function(test_state) {
  nrow(test_state$results.by_item)
}

#' @export
adapt_test.get_current_ability_estimate <- function(test_state,
                                                    opt,
                                                    estimator = opt$next_item.estimator) {
  df <- test_state$results.by_item
  if (is.null(df)) {
    res <- opt$next_item.prior_par[1]
    attr(res, "sem") <- opt$next_item.prior_par[2]
  } else {
    n <- nrow(df)
    col <- paste0("ability_", estimator)
    col_sem <- paste0("ability_", estimator, "_sem")
    res <- df[n, col]
    attr(res, "sem") <- df[n, col_sem]
  }
  stopifnot(is.scalar.numeric(res))
  res
}

adapt_test.new_state <- function() {
  list(results.by_item = NULL,
       terminate_test = FALSE)
}

adapt_test.check_inputs <- function(label, item_bank, show_item) {
  stopifnot(
    is.scalar.character(label),
    is.data.frame(item_bank),
    is.function(show_item)
  )
  for (col in c("discrimination", "difficulty", "guessing", "inattention")) {
    if (!col %in% names(item_bank)) {
      stop("column ", col, " not found in <item_bank>")
    }
    if (!is.numeric(item_bank[[col]])) {
      stop(col, " must be numeric")
    }
  }
}

adapt_test.setup <- function(label, opt) {
  code_block(function(state) {
    assert_global_is_null("adapt_test", state)
    test_state <- adapt_test.new_state()
    set_global(key = "adapt_test", value = test_state, state = state)
    register_next_results_section(state, label)
  })
}

# Returns true if we should stop
adapt_test.check_stopping_rule <- function(stopping_rule) {
  function(state) {
    test_state <- get_global(key = "adapt_test", state = state)
    stopping_rule(test_state) || test_state$terminate_test
  }
}

adapt_test.select_next_item <- function(item_bank, opt) {
  code_block(
    function(state, ...) {
      test_state <- get_global("adapt_test", state)
      ability_estimate <- adapt_test.get_current_ability_estimate(
        test_state, opt = opt, estimator = opt$next_item.estimator)
      next_item <- tryCatch(catR::nextItem(
        itemBank = item_bank[, c("discrimination", "difficulty",
                                 "guessing", "inattention")],
        theta = ability_estimate,
        out = test_state$results.by_item[, "item_id"],
        x = test_state$results.by_item[, "score"],
        criterion = opt$next_item.criterion,
        method = opt$next_item.estimator,
        maxItems = Inf,
        cbControl = opt$cb_control,
        cbGroup = opt$cb_group
      ), error = function(e) NULL)
      test_state$next_item <- next_item
      if (is.null(next_item) ||
          is.null(next_item$item) ||
          is.na(next_item$item)) {
        shiny::showNotification(
          "Failed to select new item, terminating adaptive procedure.")
        test_state$terminate_test <- TRUE
        skip_n_pages(state, n = 2L)
      } else if (admin(state)) {
        msg <- shiny::p("New item difficulty: ",
                        shiny::strong(format(next_item$par[2],
                                             digits = 3,
                                             nsmall = 3)))
        shiny::showNotification(msg, duration = NULL)
      }
      set_global(key = "adapt_test", value = test_state, state = state)
    })
}

adapt_test.administer_next_item <- function(item_bank, show_item) {
  reactive_page(
    function(state, ...) {
      test_state <- get_global("adapt_test", state)
      item_id <- test_state$next_item$item
      stopifnot(is.scalar.numeric(item_id),
                item_id > 0, item_id <= nrow(item_bank))
      show_item(item_bank[item_id, ])
    }
  )
}

adapt_test.save_result <- function(item_bank, opt) {
  code_block(
    function(state, ...) {
      test_state <- get_global("adapt_test", state)

      item_info <- test_state$next_item
      item_id <- item_info$item
      answer <- answer(state)
      correct_answer <- item_bank[item_id, "correct_answer"]
      score <- answer == correct_answer

      new_row <- data.frame(
        num = nrow(test_state$results.by_item) + 1L,
        item_id = item_id,
        discrimination = item_info[["discrimination"]],
        difficulty = item_info[["difficulty"]],
        guessing = item_info[["guessing"]],
        inattention = item_info[["inattention"]],
        information = item_info$info,
        criterion = item_info$criterion,
        answer = answer,
        correct_answer = correct_answer,
        score = score,
        ability = as.numeric(NA),
        ability_sem = as.numeric(NA)
      )

      test_state$results.by_item <- rbind(test_state$results.by_item, new_row)

      tmp_item_params <- test_state$results.by_item[, c("discrimination",
                                                        "difficulty",
                                                        "guessing",
                                                        "inattention")]
      tmp_scores <- test_state$results.by_item[, "score"]
      n <- nrow(test_state$results.by_item)
      test_state$num_items_administered <- n

      for (method in c("ML", "BM", "EAP", "WL")) {
        tmp_ability <- catR::thetaEst(tmp_item_params, tmp_scores, method = method)
        tmp_ability_sem <- catR::semTheta(thEst = tmp_ability,
                                          it = tmp_item_params,
                                          method = method)
        test_state$results.by_item[n, paste0("ability_",
                                             method)] <- tmp_ability
        test_state$results.by_item[n, paste0("ability_",
                                             method,
                                             "_sem")] <- tmp_ability_sem
      }

      if (admin(state)) {
        msg <- shiny::div(
          shiny::p(shiny::strong(if (score) "Correct" else "Incorrect"),
                   " answer."),
          shiny::p(
            "New ability estimate: ",
            shiny::strong(format(adapt_test.get_current_ability_estimate(
              test_state, opt, opt$next_item.estimator),
              digits = 3,
              nsmall = 3)))
        )
        shiny::showNotification(msg, duration = NULL,
                                type = if (score) "message" else "error")
      }

      set_global(key = "test_state", value = test_state, state = state)
    }
  )
}

adapt_test.finalise <- function(opt) {
  code_block(function(state, ...) {
    test_state <- get_global("adapt_test", state)
    df <- test_state$results.by_item
    n <- nrow(df)
    final_ability <- df[n, paste0("ability_", opt$final_ability.estimator)]
    final_ability_sem <- df[n, paste0("ability_",
                                      opt$final_ability.estimator,
                                      "_sem")]
    answer(state) <- list(ability = final_ability,
                          ability_sem = final_ability_sem)
    save_result(state, label = "ability", value = final_ability,
                metadata = list(results = df, options = opt))
    save_result(state, label = "ability_sem", value = final_ability_sem,
                metadata = NULL)
    save_result(state, label = "num_items", value = n, metadata = NULL)
    set_global("adapt_test", value = NULL, state = state)
  })
}
