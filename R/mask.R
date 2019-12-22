#' Blanks out part of the string
#'
#' @param .string A string object
#' @param pattern A regular expression to match
#' @param ... Further formatting options, passed to \code{\link{txt_style}}
#'
#' @export
mask <- function(.string, pattern, ...) {

  mask_regexp(.string, fixed(pattern), ...)

}

#' @rdname mask
#' @export
mask_regexp <- function(.string, pattern, ...) {

  hlt_regexp(.string, pattern, color = "transparent", ...)

}
