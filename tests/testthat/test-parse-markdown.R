context("parse-markdown")

test_that("examples", {
  expect_equal(parse_markdown("Hello"), "Hello")
  expect_equal(parse_markdown("Hello you"), "Hello you")
  expect_equal(parse_markdown("Hello *you*"), "Hello <em>you</em>")
  expect_equal(parse_markdown("Hello **you**"), "Hello <strong>you</strong>")
  expect_equal(parse_markdown("Hello\\\\you"), "<p>Hello</p>\n<p>you</p>\n")
})
