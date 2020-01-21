#' Creates an object of the class \code{decorate_code}
#'
#' \code{decorate_code} objects are evaluated R code, returned from \code{evaluate::evaluate}, with an attached attribute called \code{print_string} which sets up fancy formatting for knitting.
#'
#'
#' @param text A string, presumably representing R code.
#' @param eval A boolean specifying whether the code should be immediately evaluated, in addition to creating the \code{decorate_code} object. (Defaults to \code{TRUE})
#' @param collapse Should the source code be printed in one block, before the output is printed? (Defaults to \code{FALSE})
#'
#'
#' @return A \code{decorate_code} object.
#'
#' @seealso \code{\link{flair}}
#'
#' @examples
#'
#' # When run in console, this will print only the results of mean(1:10)
#' my_code <- decorate_code(text = 'mean(1:10)') %>% flair_funs()
#'
#' # The decorate_code object itself has no output
#'
#' my_code
#'
#' # However, it comes with an attribute with the decorated source code.
#'
#' attr(my_code, "print_string")
#'
#' # Objects defined by decorate_code are created in the current environment for later use.
#'
#' decorate_code('foo <- mean(1:10)')
#'
#' foo + 5
#'
#' @importFrom stringr str_trim str_detect str_replace_all
#' @importFrom purrr map map_lgl quietly
#'
#' @export
decorate_code <- function(text, eval = TRUE, collapse = FALSE) {

  #### Initial processing ####

  # remove trailing whitespace
  text <- str_trim(text)

  # evaluate code
  my_deco_code <- quietly(evaluate::evaluate)(text)$result

  # drop blank lines or invalid code
  valid <- map_lgl(my_deco_code, ~ (class(.x) != "source") || str_detect(.x, "[^\\s]+"))

  my_deco_code <- my_deco_code[valid]

  # detect source strings
  is_src <- map(my_deco_code, class) == "source"

  # scope and run it

  if (eval) {

    map(my_deco_code[is_src],
        ~quietly(scope_and_run)(.x))

}

# unlist sources and drop classes

my_deco_code[is_src] <- my_deco_code[is_src] %>%
  unlist()


attributes(my_deco_code) <- NULL

#### Shatter code into segments ####

if (collapse) {

  my_deco_code <- c(text,
                     my_deco_code[!is_src])

  attr(my_deco_code, "where_sources") <- 1

} else {


  attr(my_deco_code, "where_sources") <- which(is_src)

}

#### Assign class and attributes ####


attr(my_deco_code, "class") <- "decorate_code"

#attr(my_deco_code, "origin") <- "direct_string"

attr(my_deco_code, "eval") <- eval

get_doc_type <- purrr::safely(rmarkdown::all_output_formats)(knitr::current_input())

if (!is.null(get_doc_type$error)) {

  attr(my_deco_code, "doc_type") <- "active_source"

} else {

  attr(my_deco_code, "doc_type") <- get_doc_type$result

}

return(my_deco_code)

}



#' S3 method for knitting a \code{decorate_code} object
#'
#' @importFrom purrr map
#'
#' @export
knit_print.decorate_code <- function(x, ...) {

  eval <- attr(x, "eval")

  where_sources <- attr(x, "where_sources")

  x[-where_sources] <- purrr::map(x[-where_sources], function(val) knitr:::wrap(val, ...))

  x[where_sources] <- purrr::map(x[where_sources], function(val) wrap_source(val, attr(x, "doc_type"), ...))

  if (!eval) {

    x <- x[where_sources]

  }

  x <- x %>%
    str_c(collapse = " ")


  knitr::asis_output(x)

  #knitr::knit_print(unclass(x))

}

#' Helper for \code{knit_print.decorate_code}
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

#' S3 method for printing a \code{decorate_code} object
#'
#' Prints nothing directly; \code{decorate_code} objects should be seen and not heard.
#'
#' However, when \code{eval = TRUE} was specified, the original source code should be run and output printed.
#'
#' @export
print.decorate_code <- function(x, ...) {

  # if code is being supplied as an input object, run things, with objects defined in global environment

  if (attr(x, "eval") && !isTRUE(getOption('knitr.in.progress'))) {

    where_sources <- attr(x, "where_sources")

    purrr::map(x[-where_sources], print)

  }

  #print(x)

}

