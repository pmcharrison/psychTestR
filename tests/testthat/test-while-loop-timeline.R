context("while_loop_timeline")

test_that("main", {
  app <- AppTester$new("apps/while-loop-timeline")

  app$click_next()
  app$expect_ui_text("Page 1 Next")

  app$click_next()
  app$expect_ui_text("Page 2 Next")

  app$click_next()
  app$expect_ui_text("Page 3 Next")

  app$click_next()
  app$expect_ui_text("Demo complete.")

  app$stop()
})
