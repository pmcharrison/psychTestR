context("while_loop")

test_that("main", {
  skip_on_cran()

  app <- AppTester$new("apps/while-loop")

  app$expect_ui_text("Keep looping? Yes No")

  app$set_inputs(Yes = "click")
  app$expect_ui_text("Keep looping? Yes No")

  app$set_inputs(Yes = "click")
  app$expect_ui_text("Keep looping? Yes No")

  app$set_inputs(No = "click")
  app$expect_ui_text("End.")

  app$get_results() %>%
    as.list() %>%
    unlist() %>%
    as.character() %>%
    expect_equal(c("Yes", "Yes", "No"))

  app$stop()
})
