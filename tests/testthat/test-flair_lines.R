test_that("flair_lines highlights only some lines", {
  test_text <- 'This is line 1.
This is line 2.
This is line 3.
This is line 4.'
  res_text <- "This is line 1.<br><span style=\"background-color:#ffff7f\">This is line 2.</span><br>This is line 3.<br><span style=\"background-color:#ffff7f\">This is line 4.</span>"
  expect_equal(flair_lines(test_text, c(2,4)), res_text)
})


test_that("flair_lines works on decorated objects", {
  test_wf <- decorate_code('ggplot(iris, aes(x = Sepal.Length)) +
  geom_histogram()', eval = FALSE)

  test_result <- flair_lines(test_wf, c(2:4))

  good_str <- "ggplot(iris, aes(x = Sepal.Length)) +<br><span style=\"background-color:#ffff7f\">  geom_histogram()</span>"

  expect_equal(unclass(test_result[[2]]), good_str)
  expect_s3_class(test_result[[2]], "decorated_source")
  expect_s3_class(test_result, "decorated")
})
