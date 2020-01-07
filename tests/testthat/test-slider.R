context("text_slider")

test_that("main", {
  app <- AppTester$new("apps/slider")

  app$set_inputs(slider = 75)
  app$click_next()
  app$expect_ui_text("You gave a value of 75.")
  app$get_results() %>% as.list() %>% expect_equal(list(results = list(slider = 75)))
  app$get_locals()$x %>% expect_equal(42)

})
