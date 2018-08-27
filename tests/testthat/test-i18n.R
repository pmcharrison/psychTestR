context("i18n")

test_that("i18_dict", {
  csv <- data.frame(
    key = c("A", "B", "C"),
    GB = c("Hello", "Hi", "Goodbye"),
    FR = c("Bonjour", "Salut", "Au revoir"),
    stringsAsFactors = FALSE
  )
  x <- i18_dict$new(csv)
  expect_equal(x$as.data.frame(), csv)
  expect_equal(x$translate("A", "GB"), "Hello")
  expect_equal(x$translate("A", "FR"), "Bonjour")
  expect_error(x$translate("A", "JP"))
  expect_error(x$translate("D", "GB"))
  expect_equal(x$translate("test", "XXX", allow_missing = TRUE), "test")
})


