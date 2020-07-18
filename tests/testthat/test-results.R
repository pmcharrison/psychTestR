context("test_results")

test_that("repository_local", {
  repo <- LocalRespository$new()
  opt <- demo_options(repository = repo)
  tmp_dir <- tempfile("dir")
  R.utils::mkdirs(tmp_dir)
  withr::with_dir(tmp_dir, repo$check(opt))
})

test_that("main", {
  skip_on_cran()

  run_participant <- function(answers) {
    app <- AppTester$new("apps/results")

    for (answer in answers) {
       app$click(answer)
    }

    app
  }

  run_participants <- function(...) {
    answers <- list(...)
    lapply(answers, run_participant)
  }

  apps <-
    run_participants(c(1, 2, 3, 4),
                     c(2, 4),
                     c(4, 4, 3),
                     c(3, 3, 1, 2))

  df <- df_all_results("apps/results/output/results")

  expect_equal(df$session.complete, c(TRUE, FALSE, FALSE, TRUE))
  expect_equal(df$results.q1, c(1, 2,  4,  3) %>% as.character())
  expect_equal(df$results.q2, c(2, 4,  4,  3) %>% as.character())
  expect_equal(df$results.q3, c(3, NA, 3,  1) %>% as.character())
  expect_equal(df$results.q4, c(4, NA, NA, 2) %>% as.character())

  # Try deleting results
  app <- apps[[1]]
  app$click("admin_login_trigger")
  Sys.sleep(0.2)
  app$set_inputs(admin_password = "demo")
  app$click("submit_admin_password")
  app$executeScript("skip_confirm = true")
  app$click("admin_panel.delete_results")
  expect_equal(nrow(df_all_results("apps/results/output/results")), 0)

  deleted_zip <- normalizePath(list.files("apps/results/output/deleted-results",
                                          pattern = "\\.zip$",
                                          full.names = TRUE))
  tmp_dir <- tempfile("dir")
  R.utils::mkdirs(tmp_dir)
  withr::with_dir(tmp_dir, unzip(zipfile = deleted_zip))

  recovered_df <- df_all_results(file.path(tmp_dir, "results"))
  expect_equal(df, recovered_df)

  # Clean up
  lapply(apps, function(x) x$stop())
})
