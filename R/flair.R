#' Adds decorative formatting (flair) to parts of a string or source code.
#'
#' \code{flair} returns a string with formatting wrappers (currently only html)
#'
#' @param x A string object
#' @param pattern A pattern to match.  By default, this is a fixed pattern; use \code{flair_rx} for regular expressions.
#' @param ... Formatting style options, passed to \code{\link{txt_style}}
#'
#' @return A string with formatting wrappers.
#'
#' @examples
#'
#' code_string <- "foo <- mean(1:10, na.rm = TRUE)"
#'
#' code_string %>% flair("foo")
#'
#' code_string %>% flair_args()
#'
#' code_string %>% flair_funs(color = "red")
#'
#' @import stringr
#'
#' @rdname flair
#'
#' @export
flair_rx <- function(x, pattern, ...)  {
  UseMethod("flair_rx")
}

#' S3 method for \code{\link{decorate_code}} objects
#'
#' Applies flair to the \code{print_string} attribute.
#'
#' @param x An object of class \code{\link{decorate_code}}.
#'
#' @return An object of class \code{\link{decorate_code}}.
#'
#' @importFrom purrr map
#'
#' @export
flair_rx.decorate_code = function(x, pattern, code = TRUE, ...) {

  where_sources <- attr(x, "where_sources")

  source_strings <- purrr::map(x[where_sources], function(cs) flair_rx(cs, pattern, code, ...))

  x[where_sources] <- source_strings

  return(x)

}

#' Default S3 method for \code{\link{flair_rx}}.
#' @export
flair_rx.default <- function(x, pattern, code = TRUE, ...) {
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
  # (start of string or >) then (no < unless part of <- or <-- assignments) then (end of string or <)

  which_tags <- split_string %>% str_detect("\\<[^\\-]") %>% unlist()

  x <- purrr::map_if(split_string, !which_tags, function(x) flair_quick(x, pattern, ...)) %>%
    unlist() %>%
    str_c(collapse = "")

  return(x)
}

#' @rdname flair
#' @export
flair_quick <- function(x, pattern, ...){

  if (length(list(...)) == 0) {
    x <- x %>% str_replace_all(pattern, txt_background)
  } else {
    x <- x %>% str_replace_all(pattern, function(x) txt_style(x, ...))
  }

  return(x)
}

#' @rdname flair
#' @export
flair <- function(x, pattern, ...) {

  flair_rx(x, fixed(pattern), ...)

}

#' @rdname flair
#' @export
flair_all <- function(x, ...) {

  flair_rx(x, ".+", ...)

}



#' @rdname flair
#' @export
flair_args <- function(x, ...) {

  ## argument names should always immediately follow an open parentheses or comma space, and immediately preceed a space equals
  # allows alphanumerics, _, and . in value name
  # Preceeded by:
  # Succeeded by: closed paren or comma
  arg_regexp <- "(?<=(\\(|, ?))([:alnum:]|_|\\.)+(?= ?\\=)"

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

#' #' @rdname flair
#' #' @export
#' flair_lines <- function(x, lines, ...) {
#'
#'   x_split <- split_sandwiches(x, "(\\n|\\<br\\>)+")
#'
#'   lines <- lines
#'
#'
#' }
