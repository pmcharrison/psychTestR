#' App tester
#'
#' This is an R6 class for running automated evaluations of psychTestR tests
#' using the \code{shinytest} framework.
#' When you create an \code{AppTester} object,
#' it spins up a Shiny app and a headless web browser,
#' which you can then interact with by calling the methods of the
#' \code{AppTester} object.
#'
#' - \code{app <- AppTester$new(dir)}:
#' creates a new \code{AppTester} object. The argument
#' \code{dir} should be a directory containing a file \code{app.R},
#' with \code{app.R} containing your psychTestR test.
#'
#' - \code{app$stop()} - stops the daemon and deletes the output directory.
#' Call this once you're finished with the \code{AppTester} object.
#'
#' - \code{app$click(buttons)} - clicks all the buttons with IDs
#' listed in the character vector \code{buttons}.
#'
#' - \code{app$click_next()} - clicks the "Next" button.
#'
#' - \code{app$get_ui()} - returns the HTML of the current UI of the psychTestR test.
#'
#' - \code{app$get_ui_text()} - extracts displayed text from \code{app$get_ui()}
#' using some heuristics. If \code{squish = TRUE}, extra whitespace
#' is removed.
#'
#' - \code{app$expect_ui_text(text, squish = TRUE)} - throws an error
#' if the output of \code{app$get_ui_text(squish = squish)}
#' is not equal to the argument \code{text}.
#'
#' - \code{app$get_locals()} - returns the current local variables.
#'
#' - \code{app$get_globals()} - returns the current global variables.
#'
#' - \code{app$get_title()} - returns the current title.
#'
#' - \code{app$expect_title(text)} - throws an error
#' if the output of \code{app$get_title()}
#' is not equal to the argument \code{text}.
#'
#' - \code{app$get_problems_info()} - returns the "problems info"
#' text displayed at the bottom of the page.
#'
#' - \code{app$expect_problems_info(text)} - throws an error
#' if the output of \code{app$get_problems_info()}
#' is not equal to the argument \code{text}.
#'
#' - \code{app$get_results()} - returns the current results for the current participant.
#'
#' - \code{app$expect_results(val)} - throws an error
#' if the output of \code{as.list(app$get_results())}
#' is not equal to \code{val}.
#'
#' - \code{app$clean_output()} - deletes the output directory.
#'
#' - \code{app$set_inputs(formA = "Text A", formB = "Text B")} -
#' passes (for example) the string \code{"Text A"} to the UI
#' input with ID \code{"formA"} and the string \code{"Text B"}
#' to the UI input with ID \code{"formB"}.
#'
#' - \code{...} - \code{AppTester} objects inherit from \code{\link[shinytest]{ShinyDriver}}
#' objects, and have access to all of their methods.
#'
#' @md
#'
#' @note
#' \code{AppTester} depends on the prior installation of a headless web browser, PhantomJS,
#' which you can install with \code{\link[shinytest]{installDependencies}}.
#'
#' @export
#'
AppTester <- R6::R6Class(
  "AppTester",
  inherit = shinytest::ShinyDriver,
  public = list(

    get_ui = function() {
      self$getAllValues(input = FALSE, output = FALSE, export = TRUE)$export$ui
    },

    get_ui_text = function(squish = TRUE) {
      val <- self$get_ui() %>% as.character() %>% htm2txt::htm2txt()
      if (squish) val <- self$str_squish(val)
      val
    },

    str_squish = function(x) {
      x <- gsub("^\\s", "", x)  # trim whitespace from start of string
      x <- gsub("\\s$", "")  # trim whitespace from end of string
      x <- gsub("\\s+", " ", x)  # replace any duplicated whitespace with a single space
      x
    }

    expect_ui_text = function(text, squish = TRUE) {
      testthat::expect_equal(
        self$get_ui_text(squish = squish),
        text
      )
    },

    get_locals = function() {
      self$getAllValues(input = FALSE, output = FALSE, export = TRUE)$export$locals
    },

    get_globals = function() {
      self$getAllValues(input = FALSE, output = FALSE, export = TRUE)$export$globals
    },

    get_title = function() {
      self$getAllValues(input = FALSE, output = FALSE, export = TRUE)$export$title
    },

    expect_title = function(text) {
      testthat::expect_equal(
        self$get_title(),
        text
      )
    },

    get_problems_info = function() {
      self$getAllValues(input = FALSE, output = FALSE, export = TRUE)$export$problems_info
    },

    expect_problems_info = function(text) {
      testthat::expect_equal(
        self$get_problems_info(),
        text
      )
    },

    get_results = function() {
      self$getAllValues(input = FALSE, output = FALSE, export = TRUE)$export$results
    },

    expect_results = function(val) {
      testthat::expect_equal(
        self$get_results() %>% as.list(),
        val
      )
    },

    stop = function(clean = TRUE) {
      super$stop()
      if (clean) self$clean_output()
    },

    clean_output = function() {
      unlink(file.path(self$getAppDir(), "output"),
             recursive = TRUE)
    },

    set_inputs = function(...) {
      self$setInputs(...)
    },

    click = function(buttons) {
      call <- rep("click", times = length(buttons)) %>% setNames(buttons) %>% as.list()
      do.call(self$set_inputs, args = call)
    },

    click_next = function() {
      # self$set_inputs("next" = "click")
      self$click("next")
    }

  )
)
