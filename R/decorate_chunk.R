#' Builds a \code{\link{with_flair}} object from a code chunk
#'
#' This function reads the source code from a given named code chunk; i.e., \code{{r chunk_name, echo = FALSE}}.
#'
#' When run directly in a source file, \code{decorate_chunk()} reads the text of the active file and extracts the relevant string of source code from the chosen chunk.  (Important: this only works in RStudio.)
#'
#' When run during the \code{knitr::knit()} process, \code{decorate_chunk()} pulls the relevant chunk source during \code{knitr::knit_hooks$set("source").}
#'
#' @param chunk_name The label name of the chunk we plan to add \code{\link{flair}} to.
#' @param eval Evaluation options for chunk; behaves identically to ordinary \code{knitr} code chunk option \code{eval}
#' @param echo Evaluation options for chunk; behaves identically to ordinary \code{knitr} code chunk option \code{echo}
#' @param include Evaluation options for chunk; behaves identically to ordinary \code{knitr} code chunk option \code{include}
#'
#' @param ...  Any number of other chunk options to override.
#'
#' @return An object of class \code{\link{with_flair}}
#'
#' @importFrom stringr str_c str_trim str_remove_all
#'
#' @export
decorate_chunk <- function(chunk_name,
                        eval = TRUE,
                        echo = TRUE,
                        include = TRUE,
                        ...) {

    my_code <- NULL

    is_live <- FALSE

    try_chunk <- purrr::safely(knitr::knit_code$get)(chunk_name)


    if (is.null(try_chunk$error) && !is.null(try_chunk$result)) {

      my_code <- str_c(try_chunk$result, collapse = "\n")

      my_opts <- knitr::opts_chunk$merge(attr(try_chunk$result, "chunk_opts"))


    } else if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {

      editorIsOpen <- tryCatch({
        rstudioapi::getSourceEditorContext()
        TRUE
      }, error = function(e) FALSE)

      if (editorIsOpen) {
        ed <- rstudioapi::getSourceEditorContext()
        sources <- ed$contents

        my_opts <- knitr::opts_chunk$get()  ### fix this! ###

        my_code <- code_from_editor(sources, chunk_name)

        is_live <- TRUE

      } #if editor open

    } # chunk or editor


    if (is.null(my_code)) {

      stop(paste0("Error: No chunk found with name '", chunk_name, "'"))

    }


  # Check for flair = FALSE option
  if (!is.null(my_opts$flair) && !my_opts$flair) {

    placeholder <- list(NULL)
    attr(placeholder, "class") = "with_flair"

    return(placeholder)

  } else {

    my_code_fenced <- paste0("```{r}\n", my_code, "\n```")

    # knit just the chunk of interest
    if (is_live) {

      knitted <- knitr::knit(text = my_code_fenced,
                             quiet = TRUE)

    } else {

      # Replace OG chunk options with flairing options
      my_opts[["eval"]] <- eval
      my_opts[["echo"]] <- echo
      my_opts[["include"]] <- include

      # Combine with dots
      new_opts <- list(...)

      if (length(new_opts) > 0) {

        my_opts <- c(my_opts[!(names(my_opts) %in% names(new_opts))], new_opts)

      }

      knitted <- knitr::knit_child(text = my_code_fenced,
                                 options = my_opts,
                                 quiet = TRUE)

    }

    # convert knitted string to a list with sources separate from output
    knitted <- knitted %>% src_to_list()

    where_sources <-  map(knitted, ~attr(.x, "class")) == "source"

    attr(knitted, "class") <- "with_flair"

    attr(knitted, "orig_code_text") <- my_code

  }


  return(knitted)

}



#' Takes plain text of knitted code and converts it to a list, in which code sources have the class \code{source}.

#' @export
src_to_list <- function(knitted) {

  knitted <- knitted %>%
    split_sandwiches("```r?") %>%
    as.list()

  before_code <- which(knitted == "```r")

  knitted[before_code + 1] <- stringr::str_trim(knitted[before_code + 1])

  knitted[before_code + 1] <- purrr::map(knitted[before_code + 1], function(x) structure(list(src = x), class = "source"))

  knitted <- knitted[-c(before_code, before_code + 2)]

  return(knitted)

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
    str_c(collapse = "\n")

  attributes(chunk_text) <- NULL

  return(chunk_text)

}
