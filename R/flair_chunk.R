#' Builds a \code{\link{flair_code}} object from a code chunk
#'
#' This function reads the source code from a given named code chunk; i.e., \code{{r chunk_name, echo = FALSE}}.
#'
#' When run directly in a source file, \code{flair_chunk()} reads the text of the active file and extracts the relevant string of source code from the chosen chunk.  (Important: this only works in RStudio.)
#'
#' When run during the \code{knitr::knit()} process, \code{flair_chunk()} pulls the relevant chunk source during \code{knitr::knit_hooks$set("source").}
#'
#' @param chunk_name String that gives the name of the chunk. If left blank, current chunk is used.
#'
#' @return An object of class \code{\link{flair_code}}
#'
#' @importFrom stringr str_c str_trim
#'
#' @export
flair_chunk <- function(chunk_name) {

    sources = NULL

    try_chunk <- purrr::safely(knitr::knit_code$get)(chunk_name)

    if (is.null(try_chunk$error) && !is.null(try_chunk$result)) {

      sources <- try_chunk$result %>%
        str_c(collapse = "\n") %>%
        str_trim()

      new_flair_code <- flair_code(sources)
      attr(new_flair_code, "origin") <- "chunk-knit"

    } else if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {

          editorIsOpen <- tryCatch({
            rstudioapi::getSourceEditorContext()
            TRUE
          }, error = function(e) FALSE)

          if (editorIsOpen) {
            ed <- rstudioapi::getSourceEditorContext()
            sources <- ed$contents

            new_flair_code <- flair_code(code_from_editor(sources, chunk_name))
            attr(new_flair_code, "origin") <- "chunk-active"
          }

    }

    if (is.null(sources)) {

      stop(paste0("Error: No chunk found with name '", chunk_name, "'"))

    }

  return(new_flair_code)

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
