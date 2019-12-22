#' Wraps text in html or latex code for formatting
#'
#' \code{txt_style} adds appropriate html style wrappers to a string. Any number of options can be specified, as long as they match html CSS tags names.
#'
#' \code{txt_*} are shortcuts for specific individual style options
#'
#' Warning: These are simple direct wrappers for strings only.  If you are working with \code{demo_code} objects, you should instead use the \code{\link{highlight}} functions.
#'
#' @param x The string to be wrapped
#' @param type The style of display, defaults to "html"  (currently nothing else is supported, sorry)
#' @param bold Should the text be bolded?
#' @param underline Should the text be underlined?
#' @param italics Should the text be italicized?
#' @param ... various display options: any html CSS \code{style} options, or one of \code{font}, \code{size}, \code{color}, \code{background}, \code{style}.
#'
#' @return A string containing \code{x} with html wrappers.
#'
#' @seealso \code{\link{highlight}}
#'
#' @importFrom stringr str_c str_replace
#'
#' @examples
#' # General use
#' txt_style("I am highlighted!")
#' txt_style("I am blue and bold.", color = "blue", bold = TRUE)
#'
#' # Shortcuts
#' txt_color("I am red.")
#' txt_color("I am blue.", color = "blue")
#'
#' # Code styling wrapper
#' txt_tocode("I am code.")
#'
#' @export
txt_style <- function(x, type = "html",
                      bold = FALSE, underline = FALSE, italics = FALSE, ...) {

  # Dots to list
  my_opts <- list(...)

  # If html to correct <span> options
  if (type == "html") {


    # replacements to pass to str_replace
    opts_to_html = c("^size" = "font-size",
                     "font$" = "font-family",
                     "^style" = "font-style",
                     "background$" = "background-color")

    names(my_opts) <- names(my_opts) %>%
      str_replace(fixed("_"), "-") %>%
      str_replace_all(opts_to_html)


    # check for bold, italics, underline

    if (bold) { my_opts <- c(my_opts, "bold" = "text-weight") }

    if (italics) { my_opts <- c(my_opts, "italics" = "text-style") }

    if (underline) { my_opts <- c(my_opts, "underline" = "text-decoration") }


    # put all options into <span>
    x <- wrap_html(x, my_opts)

  }

  return(x)

}


#' @export
#' @rdname txt_style
txt_color <- function(x, color = "red"){

  txt_style(x, color = color)

}

#' @export
#' @rdname txt_style
txt_colour <- function(x, colour = "red"){
  txt_style(x, color = colour)
}

#' @export
#' @rdname txt_style
txt_size <- function(x, size = "large"){
  txt_style(x, size = size)
}

#' @export
#' @rdname txt_style
txt_background <- function(x, bg_color = "#ffff7f"){

  txt_style(x, background = bg_color)

}

## Want this to auto-lighten background color

#' @export
#' @rdname txt_style
# txt_highlight <- function(x, hlt_color){
#
#   txt_style(x, background = hlt_color)
#
# }

#' @export
#' @rdname txt_style
txt_font <- function(x, font){
  txt_style(x, font = font)
}

#' @export
#' @rdname txt_style
txt_bold <- function(x) {
  txt_style(x, bold = TRUE)
}

#' @export
#' @rdname txt_style
txt_emph <- function(x) {
  txt_style(x, italics = TRUE)
}

#' @export
#' @rdname txt_style
txt_ul <- function(x) {
  txt_style(x, underline = TRUE)
}

#' @export
#' @rdname txt_style
txt_tocode <- function(x){

  paste0("<code>", x, "</code>")

}
