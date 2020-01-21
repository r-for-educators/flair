test_that("code is scoped and run", {
  decorate("foo <- 10")

  expect_equal(foo+10, 20)
})
