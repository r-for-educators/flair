#' Creates an object of the class \code{with_flair}
#'
#' @param text A string, presumably representing R code.
#' @param ...  Any number of default chunk options to override.
#'
#'
#' @return A \code{with_flair} object.
#'
#' @seealso \code{\link{flair}}
#'
#' @examples
#'
#' # When run in console, this will print the results of mean(1:10)
#' my_code <- decorate_code(text = 'mean(1:10)') %>% flair_funs()
#'
#' # The object itself, when printed, previews your code with flair
#'
#' my_code
#'
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
decorate_code <- function(text, ...) {

  # remove trailing whitespace
  text <- str_trim(text)

  # get options
  my_opts <- knitr::opts_chunk$merge(list(...))

  # check context
  is_live <- !isTRUE(getOption('knitr.in.progress'))

  # evaluate code
  if (my_opts$eval & is_live) {

      scope_and_run(text)
      print(eval(parse(text = text)))
  }


  # Check for flair = FALSE option
  if (!is.null(my_opts$flair) && !my_opts$flair) {

    placeholder <- list(NULL)
    attr(placeholder, "class") = "with_flair"

    return(placeholder)

  } else {

    my_code_fenced <- paste0("```{r}\n", text, "\n```")

    if (is_live) {

      knitted <- knitr::knit(text = my_code_fenced,
                             quiet = TRUE)

    } else {

      knitted <- knitr::knit_child(text = my_code_fenced,
                                   options = my_opts,
                                   quiet = TRUE)
    }

    # convert knitted string to a list with sources separate from output
    knitted <- knitted %>% src_to_list()

    attr(knitted, "class") <- "with_flair"

    attr(knitted, "orig_code_text") <- text

    return(knitted)

  }

}
