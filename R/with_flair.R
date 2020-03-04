#' Create a \code{with_flair} object
#'
#' (The preferred function is \code{\link{decorate}})
#'
#' @param x A text object or code chunk label
#'
#' @returns An object of class \code{with_flair}
#'
#' @export
with_flair <- function(x) {

  decorate(x)

}

#' S3 method for knitting a \code{with_flair} object
#'
#' @param x A \code{with_flair} object.
#' @param ... Other \code{knit_print} options
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

  x[where_sources] <- map(x[where_sources], function(src) prep_source(src, doc_type))

  x <- stringr::str_c(unlist(x), collapse = "\n")


  knitr::asis_output(x)

}

#' Helper for \code{knit_print.with_flair}
#' @param x Text of source code.
#' @param doc_type Document type to knit to.
#'
#' @returns Properly wrapped text.
#'
prep_source <- function(x, doc_type) {

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

  } else if (doc_type == "github_document") {

    x <- paste0("<pre class='sourceCode r'>", txt_tocode(x), "</pre>")

  } else {

    x <- paste0("<pre><code class='language-r'>", txt_tocode(x), "</code></pre>")

  }

  return(x)

}


#' When run interactively, a \code{with_flair} object should preview the
#' flaired source code in the viewer pane. (Only if in RStudio.)
#'
#' @param x A \code{with_flair} object.
#' @param ... Other \code{print} options
#'
#' @export
print.with_flair <- function(x, ...) {

  editorIsOpen <- tryCatch({
    rstudioapi::getSourceEditorContext()
    TRUE
  }, error = function(e) FALSE)

  if (editorIsOpen) {

    tempDir <- tempfile()
    dir.create(tempDir)
    htmlFile <- file.path(tempDir, "index.html")

    where_sources <- map(x, ~attr(.x, "class")) == "source"

    x <- x[where_sources]

    x <- map(x, function(src) prep_source(src, doc_type = "unknown"))

    x <- stringr::str_c(unlist(x), collapse = "</br>")

    writeLines(x, htmlFile)

    viewer <- getOption("viewer")
    viewer(htmlFile)

  } else {

    return()

  }

}

