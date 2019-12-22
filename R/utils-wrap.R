#' Wraps text in html for formatting
#'
#' \code{wrap_html} returns a string with styling wrappers from a list of style tag names and values.
#'
#' @param x The string to be wrapped
#' @param type The style of display, defaults to "html"  (currently nothing else is supported, sorry)
#' @param opts_list The html options to include, in named vector form

#' @export
wrap_html <- function(x, opts_list){

  span_string <- stringr::str_c(names(opts_list), opts_list, sep = ":") %>%
    stringr::str_c(collapse = ";")

  x <- glue::glue("<span style='{span_string}'>{x}</span>")

  return(x)

}


### put wrap latex here when ready
