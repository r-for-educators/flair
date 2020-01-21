#' Builds a \code{\link{decorate_code}} object from either a code chunk or a string object containing source code.
#'
#' \code{decorate_code} objects are evaluated R code, returned from \code{evaluate::evaluate}, with an attached attribute called \code{print_string} which sets up fancy formatting for knitting.
#'
#' \code{decorate} does its best to guess if it has been given a code string or a chunk name, based on the presence of special characters.
#'
#' @param x A string, containing either a chunk label or R code.
#' @param eval Should this be immediately evaluated, in addition to creating the \code{decorate_code} object? (Defaults to \code{TRUE})
#' @param collapse Should the source code be printed in one block, before the output is printed? (Defaults to \code{FALSE})
#'
#' @return A \code{decorate_code} object.
#'
#' @seealso \code{\link{decorate_chunk}}, \code{\link{decorate_code}}
#'
#' @importFrom stringr str_detect
#'
#' @export
decorate <- function(x, eval = TRUE, collapse = FALSE) {

  if (!str_detect(x, "[^A-z0-9 \\-\\_]")) {

    decorate_chunk(x)

  } else {

    decorate_code(x, eval, collapse)

  }

}
