context("while_loop")

test_that("main", {
  app_dir <- "apps/while-loop"
  app <- new_app_tester(app_dir)

  expect_ui_text(app, "Keep looping? Yes No")

  set_inputs(app, Yes = "click")
  expect_ui_text(app, "Keep looping? Yes No")

  set_inputs(app, Yes = "click")
  expect_ui_text(app, "Keep looping? Yes No")

  set_inputs(app, No = "click")
  expect_ui_text(app, "End.")

  clean_output(app_dir)
})
