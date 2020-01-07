context("while_loop")

test_that("main", {
  app <- AppTester$new("apps/while-loop")

  app$expect_ui_text("Keep looping? Yes No")

  app$set_inputs(Yes = "click")
  app$expect_ui_text("Keep looping? Yes No")

  app$set_inputs(Yes = "click")
  app$expect_ui_text("Keep looping? Yes No")

  app$set_inputs(No = "click")
  app$expect_ui_text("End.")

  app$stop()
})
