#' flair: A package for adding flair to your displayed source code.
#'
#' The flair package provides ...
#'
#' @section Flair functions:
#'
#' The \code{flair_*} functions add decorative formatting to source code.
#'
#' @section Decorate functions:
#'
#' The \code{decorate} functions execute source code and simultaneously prepare it to have flair added, to display when knitted.
#'
#' @docType package
#' @name flair
NULL
## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1")  utils::globalVariables(c("."))
