test_str = "ggplot(iris, aes(x = Sepal.Length)) + geom_histogram()"
test_regexp = "Sepal\\.[:alnum:]*"

test_that("hlt_regexp works without dots", {

  good_str = "ggplot(iris, aes(x = <span style='background-color:#ffff7f'>Sepal.Length</span>)) + geom_histogram()"

  expect_equal(hlt_regexp(test_str, test_regexp), good_str)
})



test_that("hlt_regexp works with dots", {

  good_str = "ggplot(iris, aes(x = <span style='color:red;font-size:30px'>Sepal.Length</span>)) + geom_histogram()"

  expect_equal(hlt_regexp(test_str, test_regexp, color = "red", size = "30px"), good_str)
})


test_that("hlt_regexp works for flair code", {

  good_str = "ggplot(iris, aes(x = <span style='color:red;font-size:30px'>Sepal.Length</span>)) + geom_histogram()"

  test_dc <- flair_code('ggplot(iris, aes(x = Sepal.Length)) + geom_histogram()')

  test_result <- hlt_regexp(test_dc, test_regexp, color = "red", size = "30px")

  expect_equal(test_result[[1]], good_str)
  expect_equal(class(test_result), "flair_code")
})
