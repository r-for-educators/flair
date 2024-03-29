test_str = "ggplot(iris, aes(x = Sepal.Length)) + geom_histogram()"
test_regexp = "Sepal\\.[:alnum:]*"

test_that("flair_rx works without dots", {

  good_str = "ggplot(iris, aes(x = <span style=\"background-color:#ffff7f\">Sepal.Length</span>)) + geom_histogram()"

  expect_equal(flair_rx(test_str, test_regexp), good_str)
})



test_that("flair_rx works with dots", {

  good_str = "ggplot(iris, aes(x = <span style=\"color:red;font-size:30px\">Sepal.Length</span>)) + geom_histogram()"

  res_test <- flair_rx(test_str, test_regexp, color = "red", size = "30px")

  expect_equal(res_test, good_str)
})


test_that("flair_rx works for decorated object", {

  good_str = "ggplot(iris, aes(x = <span style=\"color:red;font-size:30px\">Sepal.Length</span>)) + geom_histogram()"

  test_dc <- decorate("ggplot(iris, aes(x = Sepal.Length)) + geom_histogram()", eval = FALSE)

  test_result <- flair_rx(test_dc, test_regexp, color = "red", size = "30px")

  expect_equal(unclass(test_result[[2]]), good_str)
  expect_s3_class(test_result[[2]], "decorated_source")
  expect_s3_class(test_result, "decorated")
})
