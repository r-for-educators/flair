#' Blanks out part of the string
#'
#' @param x A string object or \code{\link{decorate_code}} object.
#' @param pattern A pattern to match
#' @param before Custom preceding html tag
#' @param after Custom ending html tag
#' @param ... Further formatting options, passed to \code{\link{txt_style}}
#' @rdname mask
#' @export
mask <- function(x, pattern,
                  before = NULL, after = NULL, ...) {

  mask_rx(x, fixed(pattern), before, after, ...)

}

#' @import stringr
#' @rdname mask
#' @export
mask_rx <- function(x, pattern,
                     before = NULL, after = NULL,
                     ...)  {
  UseMethod("mask_rx")
}

#' S3 method for \code{\link{decorated}} objects
#'
#' @importFrom purrr map
#' @rdname mask
#' @export
mask_rx.decorated = function(x, pattern,
                               before = NULL, after = NULL,
                               ...) {
  x %>%
    modify_sources(
      mash_rx,
      pattern = pattern,
      before = before,
      after = after,
      ...
    ) %>%
    as_decorated()
}

#' Default S3 method for \code{\link{flair_rx}}.
#' @importFrom stringr str_extract_all str_c
#' @rdname mask
#'
#' @export
mask_rx.default <- function(x, pattern,
                             before = NULL, after = NULL,
                             ...) {
  ## Matches regular expression of pattern inside of code string
  ## Use fixed() to match exact string

  # We don't want to mask existing tags
  ## extract html tag sequences, <*>
  ## extract things between html >*<

  # rx_tags <- "(\\<[^\\<\\>]*\\>)"
  # rx_between <- "((?<=\\>|^)([^\\<]|(\\<(?=(\\-|\\<))))*(?=\\<|$))"

  split_string <- x %>%
    str_extract_all("(\\<[^\\<\\>]*\\>)|((?<=\\>|^)([^\\<]|(\\<(?=(\\-|\\<))))*(?=\\<|$))") %>%
    unlist()

  # < (not a bracket) >
  # OR
  # (start of string or >) then (no < unless part of <- or <-- assignments)
  # then (end of string or <)

  which_tags <- split_string %>% str_detect("\\<[^\\-]") %>% unlist()

  x <- purrr::map_if(split_string, !which_tags,
                     function(x) mask_quick(x, pattern,
                                             before = before, after = after, ...)) %>%
    unlist() %>%
    str_c(collapse = "")

  return(x)
}

#' @rdname mask
#' @export
mask_quick <- function(x, pattern,
                        before = NULL, after = NULL,
                        ...){

  my_styles <- list(...)

  if (!is.null(before) & !is.null(after)) {

    x <- x %>% str_replace_all(pattern, function(x) txt_tag(x, before, after))

  } else if (length(my_styles) == 0) {

    x <- x %>% str_replace_all(pattern, function(x) txt_background(x))

  }


  if (length(my_styles) != 0) {
    x <- x %>% str_replace_all(pattern, function(x) txt_style(x, ...))
  }

  x <- str_replace_all(x, pattern, word_to_blanks)

  return(x)
}


#' helper for mask
#' @param word A word to replace with blank spaces of the same length
word_to_blanks <- function(word) {

  nchar <- stringr::str_length(word)
  str_c(rep(" ", nchar), collapse = "")

}
