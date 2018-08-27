context("i18n")

test_that("i18n_dict", {
  csv <- data.frame(
    key = c("A", "B", "C"),
    GB = c("Hello", "Hi", "Goodbye"),
    FR = c("Bonjour", "Salut", "Au revoir"),
    stringsAsFactors = FALSE
  )
  x <- i18n_dict$new(csv)
  expect_equal(x$as.data.frame(), csv)
  expect_equal(x$translate("A", "GB"), "Hello")
  expect_equal(x$translate("A", "FR"), "Bonjour")
  expect_error(x$translate("A", "JP"))
  expect_error(x$translate("D", "GB"))
  expect_equal(x$translate("test", "XXX", allow_missing = TRUE), "test")
})

test_that("I18N_GLOBAL_DICT", {
  csv <- data.frame(
    key = c("A", "B", "C"),
    GB = c("Hello", "Hi", "Goodbye"),
    FR = c("Bonjour", "Salut", "Au revoir"),
    stringsAsFactors = FALSE
  )
  psychTestR:::I18N_STATE$reset()
  psychTestR:::I18N_STATE$set(dict = i18n_dict$new(csv),
                              lang = "FR")
  testthat::expect_equal(psychTestR:::I18N_STATE$translate("A"),
                         "Bonjour")
  psychTestR:::I18N_STATE$reset()
  expect_error(psychTestR:::I18N_STATE$translate("A"),
               "cannot translate, no dictionary defined")
})
