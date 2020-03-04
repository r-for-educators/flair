#' Formats source code
#'
#' Adds decorative formatting to parts of a string or source code.
#'
#' If input is a string object, \code{flair} returns a formatted string.
#'
#' If input is a \code{\link{with_flair}} object, \code{flair} returns a
#'  \code{\link{with_flair}} object with the source elements formatted.
#'
#' Currently, \code{flair} is only built for html formatting.
#'
#' @param x A string or \code{\link{with_flair}} object
#' @param pattern A pattern to match.  By default, this is a fixed pattern;
#' use \code{flair_rx} for regular expressions.
#' @param before String giving specific html tags to insert before matched text.
#' @param after String giving specific html tags to insert after matched text.
#' @param ... Formatting style options, passed to \code{\link{txt_style}}
#'
#' @return A string with formatting wrappers.
#'
#' @examples
#' \dontrun{
#' code_string <- "foo <- mean(1:10, na.rm = TRUE)"
#'
#' code_string %>% flair("foo")
#'
#' code_string %>% flair_args()
#'
#' code_string %>% flair_funs(color = "red")
#' }
#'
#' @rdname flair
#' @export
flair <- function(x, pattern,
                  before = NULL,
                  after = NULL, ...) {

  flair_rx(x, fixed(pattern), before, after, ...)

}

#' @import stringr
#' @rdname flair
#' @export
flair_rx <- function(x, pattern,
                     before = NULL, after = NULL,
                     ...)  {
  UseMethod("flair_rx")
}

#' S3 method for \code{\link{with_flair}} objects
#'
#' @importFrom purrr map
#' @rdname flair
#' @export
flair_rx.with_flair = function(x, pattern,
                               before = NULL, after = NULL,
                               ...) {

  where_sources <-  map(x, ~attr(.x, "class")) == "source"

  source_strings <- purrr::map(x[where_sources],
                               function(cs) flair_rx(cs, pattern,
                                                     before = before, after = after, ...))

  x[where_sources] <- source_strings

  x[where_sources] <- purrr::map(x[where_sources],
                                 function(x) structure(list(src = x), class = "source"))

  attr(x, "class") <- "with_flair"

  return(x)

}

#' Default S3 method for \code{\link{flair_rx}}.
#' @importFrom stringr str_extract_all str_c
#' @rdname flair
#'
#' @export
flair_rx.default <- function(x, pattern,
                             before = NULL, after = NULL,
                              ...) {
  ## Matches regular expression of pattern inside of code string
  ## Use fixed() to match exact string

  # We don't want to flair existing tags
  ## extract html tag sequences, <*>
  ## extract things between html >*<

  # rx_tags <- "(\\<[^\\<\\>]*\\>)"
  # rx_between <- "((?<=\\>|^)([^\\<]|(\\<(?=(\\-|\\<))))*(?=\\<|$))"

  split_string <- x %>%
    str_extract_all("(\\<[^\\<\\>]*\\>)|((?<=\\>|^)([^\\<]|(\\<(?=(\\-|\\<))))*(?=\\<|$))") %>%
    unlist()

  # < (not a bracket) >
  # OR
  # (start of string or >) then (no < unless part of <- or <-- assignments)
  # then (end of string or <)

  which_tags <- split_string %>% str_detect("\\<[^\\-]") %>% unlist()

  x <- purrr::map_if(split_string, !which_tags,
                     function(x) flair_quick(x, pattern,
                                             before = before, after = after, ...)) %>%
    unlist() %>%
    str_c(collapse = "")

  return(x)
}


#' @rdname flair
#' @export
flair_quick <- function(x, pattern,
                        before = NULL, after = NULL,
                        ...){

  my_styles <- list(...)

  if (!is.null(before) & !is.null(after)) {

    x <- x %>% str_replace_all(pattern, function(x) txt_tag(x, before, after))

  } else if (length(my_styles) == 0) {

    x <- x %>% str_replace_all(pattern, function(x) txt_background(x))

  }


  if (length(my_styles) != 0) {
    x <- x %>% str_replace_all(pattern, function(x) txt_style(x, ...))
  }

  return(x)
}



#' @rdname flair
#' @export
flair_all <- function(x, ...) {

  UseMethod("flair_all")

}

#' @rdname flair
#' @export
flair_all.default <- function(x, ...) {

  flair_quick(x, ".+", ...)

}

#' @rdname flair
#' @export
flair_all.with_flair <- function(x, ...) {

  where_sources <-  map(x, ~attr(.x, "class")) == "source"

  source_strings <- purrr::map(x[where_sources],
                               function(cs) flair_quick(cs, ".+", ...))

  x[where_sources] <- source_strings

  x[where_sources] <- purrr::map(x[where_sources],
                                 function(x) structure(list(src = x), class = "source"))

  attr(x, "class") <- "with_flair"

  return(x)

}



#' @rdname flair
#' @export
flair_args <- function(x, ...) {

  ## argument names should always immediately follow an open parentheses or
  # comma space, and immediately preceed a space equals
  # allows alphanumerics, _, and . in value name
  # Preceeded by:
  # Succeeded by: closed paren or comma
  arg_regexp <- "(?<=(\\(|,\\s?))([:alnum:]|_|\\.)+(?= ?\\=)"

  flair_rx(x, arg_regexp, ...)

}

#' @rdname flair
#' @export
flair_funs <- function(x, ...) {

  # allows alphanumerics, _, and . in value name
  # Succeeded by: open paren
  funs_regexp <- "([:alnum:]|_|\\.)+(?=\\()"

  flair_rx(x, funs_regexp, ...)

}

#' @rdname flair
#' @export
flair_input_vals <- function(x, ...) {

  # allows anything but a comma or close paren or equals or leading/trailing spaces
  # Preceeded by: equals and possibly space
  # Succeeded by: closed paren or comma
  ## OR
  # Preceeded by: open paren
  # Succeeded by: NOT an equals sign

  vars_regexp1 <- "(?<=\\= ?)[^,\\)\\= ][^,\\)\\=]*[^,\\)\\= ]*(?=(\\)|,))"
  vars_regexp2 <- "(?<=\\()[^,\\)\\= ][^,\\)\\=]*[^,\\)\\= ]*(?! ?\\=)"

  x %>%
    flair_rx(vars_regexp1, ...) %>%
    flair_rx(vars_regexp2, ...)
}


