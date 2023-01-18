#' Create a \code{decorated} object
#'
#' (The preferred function is \code{\link{decorate}})
#'
#' @param x A text object or code chunk label
#'
#' @return An object of class \code{decorated}
#'
#' @export
decorated <- function(x) {

  decorate(x)

}

new_decorated <- function(knitted, code = NULL, label = NULL) {
  # convert knitted string to a list with sources separate from output
  knitted <- knitted %>% src_to_list()

  structure(
    knitted,
    class = "decorated",
    orig_code_text = code,
    chunk_name = label
  )
}

as_decorated <- function(x, ...) {
  UseMethod("as_decorated", x)
}

#' @export
as_decorated.default <- function(x, ...) {
  stop("as_decorated() requires a list input and does not perform validation.")
}

#' @export
as_decorated.list <- function(x, ...) {
  structure(x, class = "decorated")
}

#' @export
as_decorated.decorated <- function(x, ...) {
  x
}

#' Method check
#'
#' @param x An object
#'
#' @return Whether the object is a \code{decorated} class object.
#'
#' @export
is.decorated <- function(x) inherits(x, "decorated")


#' When run interactively, a \code{decorated} object should preview the
#' flaired source code in the viewer pane. (Only if in RStudio.)
#'
#' @param x A \code{decorated} object.
#' @param ... Other \code{print} options
#'
#' @return None
#'
#' @export
print.decorated <- function(x, ...) {

  x_html <- htmltools::browsable(format(x))

  print(x_html)

  invisible(x)
}

#' @export
format.decorated <- function(x, ...) {
  x %>%
    purrr::keep(is_decorated_source) %>%
    map(prep_source, doc_type = "unknown") %>%
    unlist() %>%
    stringr::str_c(collapse = "<br />") %>%
    htmltools::HTML()
}

#' S3 method for knitting a \code{decorated} object
#'
#' @param x A \code{decorated} object.
#' @param ... Other \code{knit_print} options
#'
#' @return "as-is" html output, to be rendered when knitted
#'
#' @importFrom purrr map
#' @importFrom knitr knit_print
#'
#' @method knit_print decorated
#' @export
knit_print.decorated <- function(x, ...) {

  get_doc_type <- purrr::safely(rmarkdown::all_output_formats)(knitr::current_input())

  if (!is.null(get_doc_type$error) || is.null(get_doc_type$result)) {

    doc_type <- "unknown"

  } else {

    doc_type <- get_doc_type$result

  }

  x <- modify_sources(x, prep_source, doc_type = doc_type)

  x <- stringr::str_c(unlist(x), collapse = "\n")


  knitr::asis_output(x)

}

#' Helper for \code{knit_print.decorated}
#' @param x Text of source code.
#' @param doc_type Document type to knit to.
#'
#' @return Properly wrapped text.
#'
prep_source <- function(x, doc_type = "unknown") {

  x <- stringr::str_trim(x) %>%
    stringr::str_replace_all("(?<=\\s) ", "&nbsp;")

  if (doc_type == "pdf_document") {

    x <- str_replace_all(x, fixed("\n"), "\\")

  } else if (doc_type == "word_document") {

    stop("Knitting to word is not yet supported in flair")

  } else {

    x <- str_replace_all(x, fixed("\n"), "<br>")

  }

  #### Wrap source in appropriate code formatting tags ####

  if (doc_type == "pdf_document") {

    stop("Knitting to pdf is not yet supported in flair.")

  } else if (doc_type == "word_document") {

    stop("Knitting to pdf is not yet supported in flair.")

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

