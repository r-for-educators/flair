#' Wraps text in html for formatting
#'
#' \code{wrap_html} returns a string with styling wrappers from a list of style
#' tag names and values.
#'
#' @param x The string to be wrapped
#' @param opts_list The html options to include, in named vector form
#' @param type The style of display, defaults to "html"
#' (currently nothing else is supported, sorry)
#' @param class CSS classes applied to the `<span>` tag wrapping the text.
#'
#' @export
wrap_html <- function(x, opts_list, type = "html", class = NULL){

  span_string <- stringr::str_c(names(opts_list), opts_list, sep = ":") %>%
    stringr::str_c(collapse = ";")

  span_style_string <- if (nzchar(span_string)) {
    glue::glue('style="{span_string}"')
  }

  span_class_string <- if (!is.null(class) && any(!is.na(class)) && any(nzchar(class))) {
    class <- class[!is.na(class) & nzchar(class)]
    class <- paste(class, collapse = " ")
    glue::glue('class="{class}"')
  }

  span_attributes <- paste(c("", span_class_string, span_style_string), collapse = " ")
  x <- glue::glue("<span{span_attributes}>{x}</span>")

  return(x)

}
