#' Highlights parts of a string
#'
#' \code{hlt_*} returns an string of R code with formatting wrappers (currently only html)
#'
#' @param .string A string object
#' @param pattern A regular expression to match
#' @param code Should this string be displayed in R code format?
#' @param hlt_color Color to highlight code with.  Defaults to
#' @param ... Formatting options, passed to \code{\link{txt_style}}
#'
#' @return A string with formatting wrappers.
#'
#' @examples
#'
#' code_string <- "foo <- mean(1:10, na.rm = TRUE)"
#'
#' code_string %>% hlt_args()
#'
#' code_string %>% hlt_funs(color = "red")
#'
#' code_string %>% hlt_regexp("foo")
#'
#' @import stringr
#'
#' @rdname highlight
#'
#' @export
hlt_regexp <- function(.string, pattern, code = TRUE, ...)  {
  UseMethod("hlt_regexp")
}

#' S3 method for \code{\link{flair_code}} objects
#'
#' Applies highlighting to the \code{print_string} attribute.
#'
#' @param x An object of class \code{\link{flair_code}}.
#'
#' @return An object of class \code{\link{flair_code}}.
#'
#' @importFrom purrr map
#'
#' @export
hlt_regexp.flair_code = function(x, ...) {

  where_sources <- attr(x, "where_sources")

  source_strings <- purrr::map(x[where_sources], function(cs) hlt_regexp(cs, ...))

  x[where_sources] <- source_strings

  return(x)

}

#' Default S3 method for \code{\link{hlt_regexp}}.
#' @export
hlt_regexp.default <- function(.string, pattern, code = TRUE, ...) {
  ## Matches regular expression of pattern inside of code string
  ## Use fixed() to match exact string

  # We don't want to highlight existing tags
  ## extract html tag sequences, <*>
  ## extract things between html >*<

  # rx_tags <- "(\\<[^\\<\\>]*\\>)"
  # rx_between <- "((?<=\\>|^)([^\\<]|(\\<(?=(\\-|\\<))))*(?=\\<|$))"

  split_string <- .string %>%
    str_extract_all("(\\<[^\\<\\>]*\\>)|((?<=\\>|^)([^\\<]|(\\<(?=(\\-|\\<))))*(?=\\<|$))") %>%
    unlist()

  # < (not a bracket) >
  # OR
  # (start of string or >) then (no < unless part of <- or <-- assignments) then (end of string or <)

  which_tags <- split_string %>% str_detect("\\<[^\\-]") %>% unlist()

  .string <- purrr::map_if(split_string, !which_tags, function(x) hlt_quick(x, pattern, ...)) %>%
    unlist() %>%
    str_c(collapse = "")

  return(.string)
}

#' @rdname highlight
#' @export
hlt_quick <- function(.string, pattern, ...){

  if (length(list(...)) == 0) {
    .string <- .string %>% str_replace_all(pattern, txt_background)
  } else {
    .string <- .string %>% str_replace_all(pattern, function(x) txt_style(x, ...))
  }

  return(.string)
}

#' @rdname highlight
#' @export
hlt_all <- function(.string, ...) {

  hlt_regexp(.string, ".+", ...)

}

#' @rdname highlight
#' @export
hlt_fixed <- function(.string, pattern, ...) {

  hlt_regexp(.string, fixed(pattern), ...)

}

#' @rdname highlight
#' @export
hlt_args <- function(.string, ...) {

  ## argument names should always immediately follow an open parentheses or comma space, and immediately preceed a space equals
  # allows alphanumerics, _, and . in value name
  # Preceeded by:
  # Succeeded by: closed paren or comma
  arg_regexp <- "(?<=(\\(|, ?))([:alnum:]|_|\\.)+(?= ?\\=)"

  hlt_regexp(.string, arg_regexp, ...)

}

#' @rdname highlight
#' @export
hlt_funs <- function(.string, ...) {

  # allows alphanumerics, _, and . in value name
  # Succeeded by: open paren
  funs_regexp <- "([:alnum:]|_|\\.)+(?=\\()"

  hlt_regexp(.string, funs_regexp, ...)

}

#' @rdname highlight
#' @export
hlt_input_vals <- function(.string, ...) {

  # allows anything but a comma or close paren or equals or leading/trailing spaces
  # Preceeded by: equals and possibly space
  # Succeeded by: closed paren or comma
  ## OR
  # Preceeded by: open paren
  # Succeeded by: NOT an equals sign

  vars_regexp1 <- "(?<=\\= ?)[^,\\)\\= ][^,\\)\\=]*[^,\\)\\= ]*(?=(\\)|,))"
  vars_regexp2 <- "(?<=\\()[^,\\)\\= ][^,\\)\\=]*[^,\\)\\= ]*(?! ?\\=)"

  .string %>%
    hlt_regexp(vars_regexp1, ...) %>%
    hlt_regexp(vars_regexp2, ...)
}
