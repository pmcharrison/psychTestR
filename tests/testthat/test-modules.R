context("test_modules")

test_that("main", {
  skip_on_cran()

  app <- AppTester$new("apps/modules")

  app$expect_ui_text("We begin in the global environment. Next")
  app$get_locals() %>% expect_equal(list(.module = NULL,
                                         .results_label = "results"))

  app$click_next()
  app$expect_ui_text("We've now entered the parent environment. Next")
  app$get_locals() %>% expect_equal(list(.module = "parent",
                                         .results_label = "parent"))

  app$click_next()
  app$expect_ui_text("In the parent environment, we define a local variable, x = 42. Next")
  app$get_locals()$x %>% expect_equal(42)

  app$click_next()
  app$expect_ui_text("Now we enter the child environment. Next")
  app$get_locals() %>% expect_equal(list(.module = "child",
                                         .results_label = "parent.child"))

  app$click_next()
  app$expect_ui_text("We can't see x any more: x is now NULL. Next")
  app$get_locals()$x %>% expect_equal(NULL)

  app$click_next()
  app$expect_ui_text("We can set it to a new value, though: x = 65. Next")
  app$get_locals()$x %>% expect_equal(65)

  app$click_next()
  app$expect_ui_text("Now we return to the parent environment. Next")
  app$get_locals() %>% expect_equal(list(.module = "parent",
                                         .results_label = "parent",
                                         x = 42))

  app$click_next()
  app$expect_ui_text("We see that x = 42 again. Next")

  app$click_next()
  app$expect_ui_text("Now we return to the global environment. Next")
  app$get_locals() %>% expect_equal(list(.module = NULL,
                                         .results_label = "results"))

  app$click_next()
  app$expect_ui_text("We see that x is is NULL again.")
  app$get_locals()$x %>% expect_equal(NULL)

  app$get_results() %>% as.list() %>% expect_equal(list(
    parent = list(x = 42),
    parent.child = list(x = 65)
  ))

  app$stop()
})
