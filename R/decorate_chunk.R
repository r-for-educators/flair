#' Builds a \code{\link{with_flair}} object from a code chunk
#'
#' This function reads the source code from a given named code chunk;
#' i.e., \code{{r chunk_name, echo = FALSE}}.
#'
#' When run directly in a source file, \code{decorate_chunk()} reads the text of
#' the active file and extracts the relevant string of source code from the chosen chunk.
#' (Important: this only works in RStudio.)
#'
#' When run during the \code{knitr::knit()} process, \code{decorate_chunk()}
#' pulls the relevant chunk source during \code{knitr::knit_hooks$set("source").}
#'
#' @param chunk_name The label name of the chunk we plan to add \code{\link{flair}} to.
#' @param eval Evaluation options for chunk;
#' behaves identically to ordinary \code{knitr} code chunk option \code{eval}
#' @param echo Evaluation options for chunk;
#' behaves identically to ordinary \code{knitr} code chunk option \code{echo}
#' @param include Evaluation options for chunk;
#' behaves identically to ordinary \code{knitr} code chunk option \code{include}
#'
#' @param ...  Any number of other chunk options to override.
#'
#' @return An object of class \code{\link{with_flair}}
#'
#' @importFrom stringr str_c str_trim str_remove_all
#'
#' @export
decorate_chunk <- function(chunk_name,
                           eval = TRUE, echo = TRUE, include = TRUE, ...) {

  my_code <- NULL

  is_live <- FALSE

  # Check to see if we're in editor context
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {

    is_live <- tryCatch({
      rstudioapi::getSourceEditorContext()
      TRUE
    }, error = function(e) FALSE)

  }

  try_chunk <- purrr::safely(knitr::knit_code$get)(chunk_name)


  # If that worked, yay we're good
  if (is.null(try_chunk$error) && !is.null(try_chunk$result)) {

    my_code <- str_c(try_chunk$result, collapse = "\n")

    my_code <- stringr::str_trim(my_code)

    my_opts <- as.list(attributes(try_chunk$result)$chunk_opts)

    # Avoid knitr's duplicate chunk label error by appending "-flaired" to the
    # chunk label before rendering with knit_child
    my_label <- paste0(my_opts[["label"]], "-flaired")
    # If the same chunk is decorated twice, or if the user by chance has labeled
    # a chunk of the same name plys "-flaired", add a random string to ensure
    # uniqueness
    if (my_label %in% knitr::all_labels()) {
      random <- sample(c(0:9, letters), 7, replace = TRUE)
      random <- paste(random, collapse = "")
      my_label <- paste0(my_label, "-", random)
    }
    # Remove the label from the chunk options. Required for properly forming
    # my_code below.
    my_opts <- within(my_opts, rm("label"))


  } else if (is_live) {  # If that failed, try the editor pull

    ed <- rstudioapi::getSourceEditorContext()
    sources <- ed$contents

    my_code <- code_from_editor(sources, chunk_name)

  }

  # If neither of those worked, error
  if (is.null(my_code)) {

    stop(paste0("Error: No chunk found with name '", chunk_name, "'"))

  }

  # In editor, don't evaluate, but return source code for preview no matter what
  # In knitting, knit it as the options suggest.
  if (is_live) {


    # Don't bother evaluating if in editor

    my_code <- stringr::str_replace(my_code, fixed("}"),
                                    ", eval = FALSE, echo = TRUE, include = TRUE}")

    knitted <- knitr::knit(text = my_code,
                           quiet = TRUE)

  } else {

    # OG chunk options take precedence over global settings
    # glob_opts  <- get_new_globals()
    #
    # if (length(glob_opts) > 0) {
    #
    #   my_opts <- c(glob_opts[!(names(glob_opts) %in% names(my_opts))], glob_opts)
    #
    # }
    #
    # return(list_to_strings(my_opts))

    # Replace OG chunk options with flairing options
    my_opts[["eval"]] <- eval
    my_opts[["echo"]] <- echo
    my_opts[["include"]] <- include


    # Options chosen in "decorate" take precedence over OG chunk options
    new_opts <- list(...)

    if (length(new_opts) > 0) {

      my_opts <- c(my_opts[!(names(my_opts) %in% names(new_opts))], new_opts)

    }

    # Check for flair = FALSE option... for now, this will just exclude flair chunks
    if (!is.null(my_opts$flair) && !my_opts$flair) {

      placeholder <- list(NULL)
      attr(placeholder, "class") = "with_flair"

      return(placeholder)

    } else {

      # If engine isn't overwritten, it's R
      if (is.null(my_opts[["engine"]])) {

        my_engine = "r"

      } else {

        my_engine <- my_opts[["engine"]]
        my_opts <- within(my_opts, rm("engine"))

      }

      # If there are special options, write them into the chunk.

      if (length(my_opts) > 1) {

        my_code <- paste0("```{", my_engine, " ", my_label, ", ",
                          toString(list_to_strings(my_opts)),
                          "}\n", my_code, "\n```")
      } else {

        my_code <- paste0("```{", my_engine,  " ", my_label, "}\n", my_code, "\n```")

      }

      knitted <- knitr::knit_child(text = my_code,
                                 quiet = TRUE)

    } # flair = FALSE or not

  } # live in editor or not

  # convert knitted string to a list with sources separate from output
  knitted <- knitted %>% src_to_list()

  where_sources <-  map(knitted, ~attr(.x, "class")) == "source"

  attr(knitted, "class") <- "with_flair"

  attr(knitted, "orig_chunk_text") <- my_code

  return(knitted)

}


#' Takes plain text of knitted code and converts it to a list, in which code
#' sources have the class \code{source}.
#'
#' @param knitted Text of knitted code
#'
#' @return A list with code sources and knitted output.
#'
#' @export
src_to_list <- function(knitted) {

  knitted <- knitted %>%
    split_sandwiches("```[A-z]*") %>%
    as.list()

  before_code <- which(stringr::str_detect(knitted, "```[A-z]+"))

  knitted[before_code + 1] <- stringr::str_trim(knitted[before_code + 1])

  knitted[before_code + 1] <- purrr::map(knitted[before_code + 1],
                                         function(x) structure(list(src = x), class = "source"))

  knitted <- knitted[-c(before_code, before_code + 2)]

  return(knitted)

}

#' Converts raw editor text to a string of code
#'
#' Raw editor text has been taken from an active RStudio session via
#' \code{rstudioapi::getSourceEditorContext()}.  Chunk delimiters and html is
#' removed, all formatting is otherwise preserved.
#'
#' @param .contents chunk contents passed from editor context
#' @param chunk_name label of chunk
#'
#' @return chunk text
#'
#' @importFrom stringr str_c str_which str_trim
#'
code_from_editor <- function(.contents, chunk_name) {


  # Find the start of the desired chunk
  chunk_regex <- paste0('\\`\\`\\`\\{[A-z]+ ', chunk_name, '(\\}|(,.*\\}))$')

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

  chunk_text <- .contents[(start_chunk):(end_chunk)] %>%
    str_c(collapse = "\n")

  attributes(chunk_text) <- NULL

  return(chunk_text)

}


#' Converts list to vector of strings
#'
#' Helper for decorate_chunk
#'
#' @param opts_list A list, presumably of chunk options
#'
#' @return A character vector
list_to_strings <- function(opts_list) {

  paste(names(opts_list), opts_list, sep = " = ")

}

#' Compares current global chunk options to defaults;
#' finds options that differ from default.
#'
#' Helper for decorate_chunk
#'
#' @return A character vector
get_new_globals <- function() {

  default_opts <- knitr::opts_chunk$get(default = TRUE)
  current_opts <- knitr::opts_chunk$get()

  names_match <- names(current_opts) %in% names(default_opts)
  keep_1 <- current_opts[!names_match]

  ol_names <- intersect(names(current_opts), names(default_opts))
  current_opts <- current_opts[ol_names]
  default_opts <- default_opts[ol_names]

  eh <- list_to_strings(current_opts) != list_to_strings(default_opts)

  keep_2 <- current_opts[eh]

  c(keep_1, keep_2)

}
