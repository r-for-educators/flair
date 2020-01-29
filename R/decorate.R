#' Builds a \code{\link{with_flair}} object from either a code chunk or a string object containing source code.
#'
#' \code{decorate} does its best to guess if it has been given a code string or a chunk name, based on the presence of special characters.
#'
#' @param x A string, containing either a chunk label or R code.
#' @param ... Chunk options to pass along
#'
#' @return A \code{with_flair} object.
#'
#' @seealso \code{\link{decorate_chunk}}, \code{\link{decorate_code}}
#'
#' @importFrom stringr str_detect
#'
#' @export
decorate <- function(x, ...) {

  if (!str_detect(x, "[^A-z0-9 \\-\\_]")) {

    decorate_chunk(x, ...)

  } else {

    decorate_code(x, ...)

  }

}
