server <- function(elts, opt, custom_admin_panel) {
  function(input, output, session) {

    if(opt$get_user_info) {

      # Get user information
      user_info <- shiny::reactive({

        # First, navigator information (browser, hardware etc.)
        navigator_info <- input$user_navigator_info

        if(!is.null(navigator_info)) {
          navigator_info <- navigator_info %>%
            jsonlite::fromJSON() %>%
            purrr::map(function(.x) {
            if (length(.x) > 0L) {
              return(list(.x))
            } else {
              return(NA)
            }
          })
        }

        # Then geolocation info
        ip_info <- input$getIP

        c(navigator_info, ip_info)

      })

      shiny::observe({
        user_info <- user_info()
        shiny::req(user_info)
        user_information(state) <- tidy_user_info(user_info)
      })
    }


    # warning("Error handling doesn't work. Remove it :(")
    # set_error_handling(opt, session, state)
    state <- STATE$new(opt)
    setup_session(state, input, output, elts, session, opt)
    output$ui <- render_ui(state, elts, opt)
    output$title <- render_title(opt, state)
    output$problems_info <- render_problems_info(opt, state)

    shiny::observeEvent(input$next_page,
                        next_page(state, input, output, elts, session, opt,
                                  triggered_by_front_end = TRUE))
    shiny::observe(demo(state) <- if (admin(state)) TRUE else opt$demo)
    admin_panel.server(state, input, output, session, opt, elts)
    if (!is.null(custom_admin_panel))
      custom_admin_panel(
        state = state, input = input, output = output, session = session,
        opt = opt)
    manage_sessions(state, opt = opt, session = session)

    if (opt$enable_admin_panel)
      shiny::outputOptions(output, "admin_panel.ui", suspendWhenHidden = FALSE)

    if (!is.null(opt$on_start_fun)) {
      if (is.null(formalArgs(opt$on_start_fun))) {
        opt$on_start_fun()
      } else {
        opt$on_start_fun(state, session)
      }
    }

    if (!is.null(opt$on_session_ended_fun))
      session$onSessionEnded(function() {
        opt$on_session_ended_fun(state, session)
      })


    shiny::exportTestValues(
      ui = get_current_elt(state, elts, opt, eval = TRUE)@ui,
      title = i18n_title(opt, state),
      problems_info = i18n_problems_info(opt, state),
      globals = state$passive$globals,
      locals = state$passive$locals,
      results = get_results(state, complete = FALSE)
    )
  }
}


render_title <- function(opt, state) {
  shiny::renderUI({
    title <- i18n_title(opt, state)
    shiny::tagList(
      shiny::tags$head(shiny::tags$title(title)),
      shiny::h4(title, style = "text-align: left; float: left; display: inline; padding: 0 0 0 20px;")
    )
  })
}

render_problems_info <- function(opt, state) {
  shiny::renderUI(i18n_problems_info(opt, state))
}

i18n_title <- function(opt, state) {
  stopifnot(is.character(opt$title))
  if (is.null(names(opt$title)))
    opt$title else {
      if (!language(state) %in% names(opt$title))
        stop("couldn't find current language in title list") else
          opt$title[[language(state)]]
    }
}

i18n_problems_info <- function(opt, state) {
  stopifnot(is.character(opt$problems_info) || !is.null(names(opt$problems_info)))
  if (is.null(names(opt$problems_info)))
    shiny::span(opt$problems_info) else {
      if (!language(state) %in% names(opt$problems_info))
        stop("couldn't find current language in problem info list") else
          opt$problems_info[[language(state)]]
    }
}

setup_session <- function(state, input, output, elts, session, opt) {
  shiny::isolate({
    if (is_test_closed(opt)) {
      closed(state) <- TRUE
    }
    max <- opt$max_num_participants
    if (!is.null(max)) {
      results <- tabulate_results(opt, include_pilot = FALSE)
      num_complete <- sum(results$complete)
      if (num_complete + 1L > max) {
        error(state) <- opt$max_participants_msg
      }
    }
    advance_to_first_page(state, input, output, elts, session, opt)
  })
}

# safe.next_page <- function(...) {
#   tryCatch(
#     next_page(...),
#     error = function(e) {
#       error(state) <- "An error occurred when trying to advance to the next page."
#     })}

next_page <- function(state, input, output, elts, session, opt,
                      triggered_by_front_end = FALSE) {
  stopifnot(is.scalar.logical(triggered_by_front_end))
  # if (triggered_by_front_end && is.null(input$last_btn_pressed)) {
  #   error(state) <- "An unexpected error occurred."
  #   return()
  # }
  elt  <- get_current_elt(state, elts, opt, eval = TRUE)
  success <- FALSE
  if (is(elt, "page")) {
    success <- try_finalise_page(elt, state, input, session, opt)
    if (!success) make_current_page_visible()
  } else if (is(elt, "code_block")) {
    execute_code_block(elt, state = state, elts = elts,
                       input = input, output = output,
                       session = session, opt = opt)
    success <- TRUE
  }
  if (success) {
    if (elt@next_elt) increment_elt_index(state)
    new_elt <- get_current_elt(state, elts, opt, eval = FALSE)
    if (is(new_elt, "code_block")) {
      return(next_page(state, input = input, output = output,
                       elts = elts, session = session,
                       opt = opt))
    }
    # } else stop("Unrecognised test element: '", class(new_elt), "'")
  }
  save_session(state, opt)
  state$refresh_ui()
}

#' Skip pages
#'
#' Skips a certain number of pages, typically in response to a
#' participant action.
#' @param state Participant's state object.
#' @param n Number of pages to skip (negative numbers skip backwards).
#' @export
skip_n_pages <- function(state, n) {
  increment_elt_index(state, by = n)
}

try_finalise_page <- function(elt, state, input, session, opt) {
  stopifnot(is(elt, "page"), is(state, "state"))
  I18N_STATE$set(dict = elt@i18n_dict, lang = language(state))
  res <- if (elt@final) {
    shinyjs::alert("Cannot advance on a 'final' page!")
    FALSE
  } else {
    perform_get_answer_function(elt, state, input, session, opt)
    if (!validate_elt(elt, state, input, session, opt)) {
      message("Input validation failed.")
      FALSE
    } else {
      if (elt@save_answer) save_result(state, elt@label, answer(state))
      perform_on_complete_function(elt, state, input, session, opt)
      TRUE
    }
  }
  I18N_STATE$reset()
  res
}

perform_get_answer_function <- function(elt, state, input, session, opt) {
  f <-  elt@get_answer
  if (!is.null(f)) {
    answer(state) <- f(
      state = state, input = input, session = session, opt = opt)
  }
}

perform_on_complete_function <- function(elt, state, input, session, opt) {
  f <- elt@on_complete
  if (!is.null(f)) f(
    state = state, answer = answer(state), input = input, session = session, opt = opt)
}

execute_code_block <- function(elt, state, elts, input, output,
                               session, opt) {
  stopifnot(is(elt, "code_block"))
  I18N_STATE$set(dict = elt@i18n_dict, lang = language(state))
  elt@fun(state = state, elts = elts, input = input, output = output,
          session = session, opt = opt, answer = answer(state))
  I18N_STATE$reset()
}

check_elts <- function(x) {
  if (x$length == 0L) {
    stop("timeline cannot have length 0")
  }
  if (length(x$languages) == 0L) {
    stop("timeline must contain at least one language")
  }
  # first_elt <- elts[[1]]
  # if (!(is(first_elt, "page") || is(first_elt, "reactive_page"))) {
  #   stop("The first element in `elts` must be a (possibly reactive) test page.")
  # }
  elts <- x$get(x$languages[1]) # Check the elements for the first available language
  last_elt <- elts[[length(elts)]]
  if (!(is(last_elt, "page") || is(last_elt, "reactive_page"))) {
    stop("The last element in `elts` must be a (possibly reactive) test page.")
  }
  if (is(last_elt, "page") && !last_elt@final) {
    stop("The last element in `elts` must be marked 'final' ",
         "(try setting `final` to `TRUE` in the last test page).")
  }
  class_check <- vapply(elts, function(x) is(x, "test_element"), logical(1))
  class_check_failed <- which(!class_check)
  if (!all(class_check))
    stop("every element of `elts` must be an object of class `test_element`, ",
         "but this was not true for ",
         ngettext(length(class_check_failed), "element ", "elements "),
         paste(class_check_failed, collapse = ", "))
}

render_ui <- function(state, elts, opt) {
  shiny::renderUI({
    state$reactive$ui_reactive_trigger
    elt <- if (!is.null(error(state))) {
      final_page(paste0("Error: ", error(state)))
    } else if (closed(state)) {
      final_page(opt$server_closed_msg)
    } else {
      get_current_elt(state, elts, opt, eval = TRUE)
    }
    ui <- if (is(elt, "page")) elt@ui else shiny::div()
    shiny::div(id = "current_page.ui", ui)
  })
}

validate_elt <- function(elt, state, input, session, opt) {
  f <- elt@validate
  res <- if (is.null(f)) TRUE else f(
    answer = answer(state), state = state, input = input, session = session,
    opt = opt)
  if (isTRUE(res)) {
    TRUE
  } else {
    msg <- if (isFALSE(res)) {
      "Invalid result."
    } else if (is.scalar.character(res)) {
      res
    } else {
      print(res)
      stop("validation function must either return `TRUE` for success or, ",
           "for failure, either `FALSE` or a character scalar error message")
    }
    shinyjs::alert(msg)
    FALSE
  }
}

make_current_page_visible <- function() {
  shinyjs::runjs("document.getElementById('current_page.ui').style.visibility = 'visible'")
}

#' Close test
#'
#' Closes the test.
#' @param opt Test options list as created by \code{test_options()}.
#' @export
close_test <- function(opt) {
  closed <- is_test_closed(opt)
  if (closed) {
    shiny::showNotification("Test is already closed.", type = "warning")
  } else {
    success <- file.create(opt$closed_file)
    if (success) {
      shiny::showNotification("Test successfully closed.", type = "message")
    } else {
      shiny::showNotification("Failed to close test.", type = "error")
    }
  }
}

#' Open test
#'
#' Opens the test.
#' @param opt Test options list as created by \code{test_options()}.
#' @export
open_test <- function(opt) {
  closed <- is_test_closed(opt)
  if (!closed) {
    shiny::showNotification("Test is already open.", type = "warning")
  } else {
    success <- file.remove(opt$closed_file)
    if (success) {
      shiny::showNotification("Test successfully opened.", type = "message")
    } else {
      shiny::showNotification("Failed to open test.", type = "error")
    }
  }
}

#' Is test closed?
#'
#' Checks whether the test is closed.
#' @param opt Test options list as created by \code{test_options()}.
#' @return \code{TRUE} if the test is closed, \code{FALSE} otherwise.
#' @export
is_test_closed <- function(opt) {
  file.exists(opt$closed_file)
}
