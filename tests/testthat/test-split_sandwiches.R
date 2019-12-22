test_that("split_sandwich pulls html apart", {

  highlighted_code <- "<code>foo <- mean(<span style='background-color:#ffff7f'>1:10</span>)</code>"

  start_rx <- "\\<([^\\>\\<]|(\\>\\s*\\<))*([^\\-])\\>"

  expected_results_1 <- c(
    "<code>",
    "foo <- mean(",
    "<span style='background-color:#ffff7f'>",
    "1:10",
    "</span>",
    ")",
    "</code>")

  expect_equal(expected_results_1, split_sandwiches(highlighted_code, start_rx))
})
