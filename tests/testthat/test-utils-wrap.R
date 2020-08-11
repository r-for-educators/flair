test_that("wrap_html() uses `class`", {
  good_str <- '<span class="flair flair-fancy" style="color:red">CODE</span>'

  expect_equal(
    wrap_html("CODE", list(color = "red"), class = c("flair", "flair-fancy")),
    good_str
  )
  expect_equal(
    wrap_html("CODE", list(color = "red"), class = c("flair", "flair-fancy", NA)),
    good_str
  )
  expect_equal(
    wrap_html("CODE", list(color = "red"), class = c("flair", "flair-fancy", NA, "")),
    good_str
  )
})

test_that("wrap_html() ignore missing `class`", {
  good_str <- '<span style="color:red">CODE</span>'

  expect_equal(wrap_html("CODE", list(color = "red")), good_str)
  expect_equal(wrap_html("CODE", list(color = "red"), class = NA), good_str)
  expect_equal(wrap_html("CODE", list(color = "red"), class = ""), good_str)
})

test_that("txt_style() passes `class` to wrap_html()", {
  expect_equal(
    txt_style("CODE", color = "red", class = c("flair", "flair-fancy")),
    '<span class="flair flair-fancy" style="color:red">CODE</span>'
  )

  expect_equal(
    txt_style("CODE", class = c("flair", "flair-fancy")),
    '<span class="flair flair-fancy">CODE</span>'
  )

  expect_equal(
    txt_style("CODE", bold=TRUE, class = c("flair", "flair-fancy")),
    '<span class="flair flair-fancy" style="text-weight:bold">CODE</span>'
  )

  expect_equal(
    txt_style("I am danger", class = "text-danger"),
    '<span class="text-danger">I am danger</span>'
  )
})
