

#' S3 method for knitting a \code{with_flair} object
#'
#' @importFrom purrr map
#'
#' @export
knit_print.with_flair <- function(x, ...) {

  get_doc_type <- purrr::safely(rmarkdown::all_output_formats)(knitr::current_input())

  if (!is.null(get_doc_type$error)) {

    doc_type = "unknown"

  } else {

    doc_type <- get_doc_type$result

  }

  where_sources <- map(x, ~attr(.x, "class")) == "source"

  x[where_sources] <- map(x[where_sources], function(src) prep_source(src, doc_type, ...))

  x <- stringr::str_c(unlist(x), collapse = "\n")


  knitr::asis_output(x)

  #knitr::knit_print(unclass(x))

}

#' Helper for \code{knit_print.with_flair}
prep_source <- function(x, doc_type, ...) {

  x <- stringr::str_trim(x) %>%
    stringr::str_replace_all("(?<=\\s) ", "&nbsp;")

  if (doc_type == "pdf_document") {

    x <- str_replace_all(x, fixed("\n"), "\\")

  } else if (doc_type == "word_document") {

    # word is dumb

  } else {

    x <- str_replace_all(x, fixed("\n"), "<br>")

  }

  #### Wrap source in appropriate code formatting tags ####

  if (doc_type == "pdf_document") {
    # figure this out
  } else if (doc_type == "word_document") {

    # also this

  } else if (doc_type == "html_document") {

    x <- paste0("<pre class='prettyprint'>", txt_tocode(x), "</pre>")

  } else if (doc_type == "ioslides_presentation") {

    x <- paste0("<pre class='prettyprint lang-r'>", txt_tocode(x), "</pre>")

  } else if (doc_type == "xaringan::moon_reader") {

    x <- paste0("<code class ='r hljs remark-code'>", x, "</code>")

  } else if (doc_type == "slidy_presentation") {

    x <- paste0("<pre class='sourceCode r'><code class='sourceCode r'>", x, "</code></pre>")

    # } else if (doc_type == "revealjs::revealjs_presentation") {
    #
    #   x <-

  } else {

    x <- paste0("<pre><code class='language-r'>", txt_tocode(x), "</code></pre>")

  }

  return(x)

}


#' No printing for u
#' @export
print.with_flair <- function(x, ...) {

  tempDir <- tempfile()
  dir.create(tempDir)
  htmlFile <- file.path(tempDir, "index.html")

  where_sources <- map(x, ~attr(.x, "class")) == "source"

  x <- x[where_sources]

  x <- map(x, function(src) wrap_source(src, doc_type = "unknown", ...))

  x <- stringr::str_c(unlist(x), collapse = "</br>")

  writeLines(x, htmlFile)

  viewer <- getOption("viewer")
  viewer(htmlFile)

}

