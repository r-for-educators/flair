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

#' weird way to get formatting wrappers
#' \code{knitr_wrap} returns the output of knitr:::wrap
#'
#' @param x evaluated input to wrap
knitr_wrap <- function(x, ...) {


  options = rlang::list2(...)$options

  fig.cur = knitr:::plot_counter()
  options$fig.cur = fig.cur # put fig num in options
  name = knitr:::fig_path('', options, number = fig.cur)

  files = mapply(
    knitr:::save_plot, width = options$fig.width, height = options$fig.height,
    dev = options$dev, ext = options$fig.ext, dpi = options$dpi,
    MoreArgs = list(plot = x, name = name, options = options), SIMPLIFY = FALSE
  )

  #c(my_opts$fig.width, my_opts$fig.height,
          #my_opts$dev, my_opts$fig.ext, my_opts$dpi)

  #my_opts
  #knitr::engine_output(options = my_opts, out = x)


}
