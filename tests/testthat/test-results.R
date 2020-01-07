context("test_results")

test_that("main", {
  skip_on_cran()

  run_participant <- function(answers) {
    app <- AppTester$new("apps/results")

    for (answer in answers) {
       app$click(answer)
    }

    invisible(app)
  }

  run_participants <- function(...) {
    for (answers in list(...)) {
      app <- run_participant(answers)
    }
    app
  }

  app <- run_participants(c(1, 2, 3, 4),
                          c(2, 4),
                          c(4, 4, 3),
                          c(3, 3, 1, 2))

  df <- df_all_results("apps/results/output/results")

  expect_equal(df$session.complete, c(TRUE, FALSE, FALSE, TRUE))
  expect_equal(df$results.q1, c(1, 2,  4,  3) %>% as.character())
  expect_equal(df$results.q2, c(2, 4,  4,  3) %>% as.character())
  expect_equal(df$results.q3, c(3, NA, 3,  1) %>% as.character())
  expect_equal(df$results.q4, c(4, NA, NA, 2) %>% as.character())

  app$stop()

})
