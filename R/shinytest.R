AppTester <- R6::R6Class(
  "AppTester",
  inherit = shinytest::ShinyDriver,
  public = list(

    get_ui_text = function(squish = TRUE) {
      val <- self$getAllValues(input = FALSE, output = FALSE, export = TRUE)$export$ui_text
      if (squish) val <- stringr::str_squish(val)
      val
    },

    expect_ui_text = function(text, squish = TRUE) {
      # Sys.sleep(0.05)
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

    click_next = function() {
      self$set_inputs("next" = "click")
    }

  )
)
