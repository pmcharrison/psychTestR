context("hash_df")

test_that("hash_df", {
  csv <- data.frame(
    key = c("A", "B", "C"),
    GB = c("Hello", "Hi", "Goodbye"),
    FR = c("Bonjour", "Salut", "Au revoir"),
    stringsAsFactors = FALSE
  )
  expect_equal(csv, unhash_df(hash_df(csv, markdown = FALSE)))
  h <- hash_df(csv, markdown = FALSE)
  expect_equal(h$A$GB, "Hello")
  expect_error(hash_df(42, markdown = FALSE))
  expect_error(hash_df(data.frame(Key = 10, GB = "Hi"), markdown = FALSE))
  expect_error(hash_df(data.frame(key = 10, GB = "Hi"), markdown = FALSE))
})
