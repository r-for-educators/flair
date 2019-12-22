#' flair: A package for adding flair to your displayed source code.
#'
#' The flair package provides ...
#'
#' @section Highlight functions:
#'
#' The \code{hlt_*} functions add display formatting to source code.
#'
#' @section Flair functions:
#'
#' The \code{flair_*} functions execute the code and prepare source code to be printed with flair when knitted.
#'
#' @docType package
#' @name flair
NULL
## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1")  utils::globalVariables(c("."))
