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
  expect_warning(psychTestR:::I18N_STATE$translate("A"),
                 "undefined i18n dictionary/language, key left untranslated")
})

test_that("i18n_app_en", {
  skip_on_cran()

  app <- AppTester$new("apps/i18n/i18n-en")

  app$expect_ui_text("Hello Next")
  app$click_next()
  app$expect_ui_text("Goodbye")
  app$expect_title("English title")
  app$expect_problems_info("English problems info")

  app$stop()
})

test_that("i18n_app_fr", {
  skip_on_cran()

  app <- AppTester$new("apps/i18n/i18n-fr")

  app$expect_ui_text("Bonjour Next")
  app$click_next()
  app$expect_ui_text("Au revoir")
  app$expect_title("French title")
  app$expect_problems_info("French problems info")

  app$stop()
})
