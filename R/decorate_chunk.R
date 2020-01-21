#' Builds a \code{\link{decorate_code}} object from a code chunk
#'
#' This function reads the source code from a given named code chunk; i.e., \code{{r chunk_name, echo = FALSE}}.
#'
#' When run directly in a source file, \code{decorate_chunk()} reads the text of the active file and extracts the relevant string of source code from the chosen chunk.  (Important: this only works in RStudio.)
#'
#' When run during the \code{knitr::knit()} process, \code{decorate_chunk()} pulls the relevant chunk source during \code{knitr::knit_hooks$set("source").}
#'
#' @param chunk_name The label name of the chunk we plan to add \code{\link{flair}} to.
#'
#' @return An object of class \code{\link{decorate_code}}
#'
#' @importFrom stringr str_c str_trim str_remove_all
#'
#' @export
decorate_chunk <- function(chunk_name,
                        eval = TRUE) {

    sources = NULL

    try_chunk <- purrr::safely(knitr::knit_code$get)(chunk_name)

    ## To do: get chunk options here and use those instead.

    if (is.null(try_chunk$error) && !is.null(try_chunk$result)) {

      sources <- try_chunk$result %>%
        str_c(collapse = "\n") %>%
        str_trim()

      new_deco <- decorate_code(sources, eval)
      attr(new_deco, "origin") <- "chunk-knit"

    } else if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {

          editorIsOpen <- tryCatch({
            rstudioapi::getSourceEditorContext()
            TRUE
          }, error = function(e) FALSE)

          if (editorIsOpen) {
            ed <- rstudioapi::getSourceEditorContext()
            sources <- ed$contents

            new_deco <- decorate_code(code_from_editor(sources, chunk_name), eval)
            attr(new_deco, "origin") <- "chunk-active"
          }

    }

    if (is.null(sources)) {

      stop(paste0("Error: No chunk found with name '", chunk_name, "'"))

    }

  return(new_deco)

}

#' Converts raw editor text to a string of code
#'
#' Raw editor text has been taken from an active RStudio session via \code{rstudioapi::getSourceEditorContext()}.  Chunk delimiters and html is removed, all formatting is otherwise perserved.
#'
#' @param .contents
#'
#' @return chunk text
#'
#' @importFrom stringr str_c str_which str_trim
#'
code_from_editor <- function(.contents, chunk_name) {


  # Find the start of the desired chunk
  chunk_regex <- paste0('\\`\\`\\`\\{r ', chunk_name, '(\\}|(,.*\\}))$')

  start_chunk <- .contents %>%
    str_which(chunk_regex)

  if (length(start_chunk) == 0) {

    stop(paste0("Error: No chunk found with name '", chunk_name, "'"))

  } else if (length(start_chunk) > 1) {

    stop(paste0("Error: Duplicate chunk name '", chunk_name, "'"))

  }

  end_chunk <- .contents[-c(1:start_chunk)] %>%
    str_which(fixed("```")) %>%
    min() + start_chunk

  chunk_text <- .contents[(start_chunk+1):(end_chunk-1)] %>%
    str_c(collapse = "\n") %>%
    str_trim()

  attributes(chunk_text) <- NULL

  return(chunk_text)

}
