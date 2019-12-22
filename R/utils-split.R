#' Separates a string into sections by delimiter
#'
#' This function takes delimiters for the beginning and (optionally, if different) end of sections of a string, and returns a vector with split string elements.
#'
#' The main use case for \code{split_sandwiches()} is for html editing: You might want to separate the original text from the html tags, make certain edits to the text only, and then re-wrap the tags.
#'
#' This is different from \code{str_split()} or similar, because the delimiters are preserved and remain attached to a section.
#'
#' Note that \code{split_sandwiches()} is not vectorized (sorry).  It only takes a single character object.
#'
#' @param .string A string
#' @param start_rx A regular expression denoting the beginning of a section.  Use \code{fixed()} for literals.
#' @param end_rx A regular expression denoting the end of a section.  If none supplied, sections end when the next \code{start_rx} is encountered.
#'
#' @return A vector of strings
#'
#' @examples
#' my_string <- "<span style='text-color:blue'> I am blue and <b>bold</b>, yay! </span>"
#'
#' split_sandwiches(my_string, "\\<[^\\>\\<]*\\>")
#'
#' @importFrom glue glue
#' @importFrom stringr str_extract_all str_subset str_length

#' @export
split_sandwiches <- function(.string, start_rx, end_rx = NULL) {


  top_buns <- str_extract_all(.string, start_rx) %>% unlist()

  if (is.null(end_rx)) {

    meat <- .string %>%
      str_split(start_rx) %>% unlist()

    bottom_buns = NULL

  } else {

    meat <- .string %>%
      str_split(start_rx) %>% unlist() %>%
      str_split(end_rx) %>% unlist()

    bottom_buns <- str_extract_all(.string, end_rx) %>% unlist()

  }


  # Check that buns are matched, or if no buns just return the string
  if (!is.null(end_rx) && length(top_buns) != length(bottom_buns)) {

    stop("Error: Each top bread must have a matching bottom bread.")

  } else if (length(top_buns) == 0) {

    return(.string)

  }

  sammie <- make_sandwiches(meat, top_buns, bottom_buns)
  sammie <- sammie[str_length(sammie) != 0]

  return(sammie)

  }


#' Recombines "sandwich" elements in proper order
#'
#' Helper for \code{\link{split_sandwiches}}.  Once sections have been identified and extracted, recombines them in the correct order.
#'
#' @param meat Character vector containing string sectiosn between delimiters
#' @param top_buns Character vector containing starting delimiters
#' @param bottom_buns Character vector containing ending delimiters
#'
#' @return A character vector.
make_sandwiches <- function(meat, top_buns, bottom_buns = NULL) {

    n_buns <- length(top_buns) + length(bottom_buns)
    n_meat <- length(meat)

    if (n_meat != n_buns + 1) {

      stop("Error: Something weird happened...")

    }

    if (is.null(bottom_buns)) {

      bread <- top_buns

    } else {

      bread <- rep("", n_buns)
      bread[((1:(n_buns/2))*2 - 1)] = top_buns
      bread[(1:(n_buns/2))*2] = bottom_buns

    }

    sammie <- rep("", n_meat + n_buns)
    where_meat <- (1:n_meat)*2 - 1
    sammie[where_meat] = meat
    sammie[-where_meat] = bread

    return(sammie)

}
