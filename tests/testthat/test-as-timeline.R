context("test-as.timeline")

test_that("various examples", {
  x <- new_timeline(list(one_button_page("Hello")))
  expect_equal(x, as.timeline(x))
  expect_equal(x,
               as.timeline(list(one_button_page("Hello"))))
  expect_error(as.timeline(3),
               regexp = "don't know how to coerce object of class numeric to a timeline")
})
