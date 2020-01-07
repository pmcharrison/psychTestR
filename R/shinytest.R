new_app_tester <- function(app_dir) {
  shinytest::ShinyDriver$new(app_dir)
}

get_ui_text <- function(app_tester, squish = TRUE) {
  val <- app_tester$getAllValues(input = FALSE, output = FALSE, export = TRUE)$export$ui_text
  if (squish) val <- stringr::str_squish(val)
  val
}

expect_ui_text <- function(app_tester, text, squish = TRUE) {
  testthat::expect_equal(
    get_ui_text(app_tester, squish = squish),
    text
  )
}

clean_output <- function(app_dir) {
  unlink(file.path(app_dir, "output"),
         recursive = TRUE)
}

set_inputs <- function(app, ...) {
  app$setInputs(...)
}
