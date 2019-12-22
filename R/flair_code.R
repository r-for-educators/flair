#' Creates an object of the class \code{flair_code}
#'
#' \code{flair_code} objects are evaluated R code, returned from \code{evaluate::evaluate}, with an attached attribute called \code{print_string} which sets up fancy formatting for knitting.
#'
#' @param .code_string A string containing executable R code OR a valid expression that will be converted to a string via \code{deparse()}
#' @param eval A boolean specifying whether the code should be immediately evaluated, in addition to creating the \code{flair_code} object. (Defaults to \code{TRUE})
#'
#' @return A \code{flair_code} object.
#'
#' @seealso \code{\link{highlight}}, \code{\link{_chunk}}
#'
#' @examples
#'
#' # When run in console, this will print only the results of mean(1:10)
#' my_flair <- flair_code('mean(1:10)') %>% hlt_funs()
#'
#' # The flair_code object itself has no output
#'
#' my_flair
#'
#' # However, when knitted, the source code is formatted.
#'
#' attr(my_flair, "print_string")
#'
#'
#' # Objects defined in flair_code are created in the environment
#'
#' flair_code('foo <- mean(1:10)')
#'
#' foo + 5
#'
#' @importFrom stringr str_trim str_detect str_replace_all
#' @importFrom purrr map map_lgl quietly
#'
#' @export
flair_code <- function(.code_string, eval = TRUE, shatter = TRUE) {

  #### Initial processing ####

  # remove trailing whitespace
  .code_string <- str_trim(.code_string)

  # evaluate code
  my_flair_code <- quietly(evaluate::evaluate)(.code_string)$result

  # drop blank lines or invalid code
  valid <- map_lgl(my_flair_code, ~ (class(.x) != "source") || str_detect(.x, "[^\\s]+"))

  my_flair_code <- my_flair_code[valid]

  # detect source strings
  is_src <- map(my_flair_code, class) == "source"

  # scope and run it

  if (eval) {

    map(my_flair_code[is_src],
        ~quietly(scope_and_run)(.x))

  }

  # unlist sources and drop classes

  my_flair_code[is_src] <- my_flair_code[is_src] %>%
    unlist()


  attributes(my_flair_code) <- NULL

  #### Shatter code into segments ####

  if (!shatter) {

    my_flair_code <- c(.code_string,
                       my_flair_code[!is_src])

    attr(my_flair_code, "where_sources") <- 1

  } else {


    attr(my_flair_code, "where_sources") <- which(is_src)

  }

  #### Assign class and attributes ####


  attr(my_flair_code, "class") <- "flair_code"

  #attr(my_flair_code, "origin") <- "direct_string"

  attr(my_flair_code, "eval") <- eval

  get_doc_type <- purrr::safely(rmarkdown::all_output_formats)(knitr::current_input())

  if (!is.null(get_doc_type$error)) {

    attr(my_flair_code, "doc_type") <- "active_source"

  } else {

    attr(my_flair_code, "doc_type") <- get_doc_type$result

  }

  return(my_flair_code)

}


#' S3 method for knitting a \code{flair_code} object
#'
#' @importFrom purrr map
#'
#' @export
knit_print.flair_code <- function(x, ...) {

  where_sources <- attr(x, "where_sources")

  x[-where_sources] <- purrr::map(x[-where_sources], function(val) knitr:::wrap(val, ...))

  x[where_sources] <- purrr::map(x[where_sources], function(val) wrap_source(val, attr(x, "doc_type"), ...))

  if (!attr(x, "eval")) {

    x <- x[where_sources]

  }

  x <- x %>%
      str_c(collapse = " ")


  knitr::asis_output(x)

  #knitr::knit_print(unclass(x))

}

#' Helper for \code{knit_print.flair_code}
wrap_source <- function(x, doc_type, ...) {

  #### reformat line breaks ####

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

#' S3 method for printing a \code{flair_code}
#'
#' Prints nothing; \code{flair_code} objects should be seen and not heard.
#'
#' If the \code{flair_code} object was created by inputting a string, we should run that code and print any output.
#'
#' @export
print.flair_code <- function(x, ...) {

  # if code is being supplied as an input object, run things, with objects defined in global environment

  if (attr(x, "eval") && !isTRUE(getOption('knitr.in.progress'))) {

    where_sources <- attr(x, "where_sources")

    purrr::map(x[-where_sources], print)

  }

  #print(x)

}
