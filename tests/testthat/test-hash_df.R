context("hash_df")

test_that("hash_df", {
  csv <- data.frame(
    key = c("A", "B", "C"),
    GB = c("Hello", "Hi", "Goodbye"),
    FR = c("Bonjour", "Salut", "Au revoir"),
    stringsAsFactors = FALSE
  )
  expect_equal(csv, unhash_df(hash_df(csv)))
  h <- hash_df(csv)
  expect_equal(h$A$GB, "Hello")
  expect_error(hash_csv(42))
  expect_error(hash_csv(data.frame(Key = 10, GB = "Hi")))
  expect_error(hash_csv(data.frame(key = 10, GB = "Hi")))
})
