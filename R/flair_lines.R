#' Adds decorative formatting (flair) to parts of a string or source code,
#' specified by line(s).
#'
#' \code{flair_lines} returns a string with formatting wrappers(currently only
#' html), or applies the formatting to the source elements of a
#' \code{\link{decorated}} object.
#'
#' @param x A string or \code{\link{decorated}} object
#' @param lines Integer vector indicating which lines to apply the flair styling to.
# @param ... Formatting style options, passed to \code{\link{txt_style}}
#'
#' @return A string with formatting wrappers.
#'
#' @examples
#'
#' code_string <- "x <- mean(1:10, na.rm = TRUE)
#' sqrt(x)" %>% flair_lines(2)
#'
#' @export
flair_lines <- function(x, lines) {

  UseMethod("flair_lines")

}

#' @rdname flair_lines
#' @export
flair_lines.default <- function(x, lines) {

  x_split <- x %>%
    stringr::str_split("(\\n|\\<br\\>)+") %>%
    unlist()

  x_split[lines] <- x_split[lines] %>% flair_all()

  return(stringr::str_c(x_split, collapse = "<br>"))


}

#' S3 method for \code{\link{decorated}} objects
#'
#' Applies flair to the appropriate line(s) of source code.
#'
#' @param x An object of class \code{\link{decorated}}.
#' @param lines An integer vector specifying code lines to highlight.
# @param ... Formatting style options, passed to \code{\link{txt_style}}
#'
#' @return An object of class \code{\link{decorated}}.
#'
#' @importFrom stringr str_split str_trim str_remove_all
#' @importFrom purrr map map2
#'
#' @export
flair_lines.decorated <- function(x, lines) {

  where_sources <- is_decorated_source(x)

  line_nums <-
    x[where_sources] %>%
    map(~str_count(.x, "\\n|(<br>)")) %>%
    map2(1:sum(where_sources), ~ 0:.x + .y)

  to_flair <- unlist(map(line_nums, ~any(.x %in% lines)))

  source_strings <- purrr::map2(x[where_sources][to_flair],
                                line_nums[to_flair],
                                 ~flair_sublines(.x, .y, lines))

  x[where_sources][to_flair] <- source_strings

  x[where_sources][to_flair] <- purrr::map(x[where_sources][to_flair], as_decorated_source)

  #x <- c(x, script)

  as_decorated(x)

}


#' Helper for \code{flair_lines}
#'
#' @param text Text from knitted source code
#' @param nums List of overall lines contained
#' @param lines Which lines to highlight
#'
#' @return Text with formatting wrappers
#'
#' @importFrom stringr str_split str_c
flair_sublines <- function(text, nums, lines) {
  which_lines <- which(nums %in% lines)
  text <- text %>% str_split("\\n|(<br>)") %>% unlist()
  text[which_lines] <- map(text[which_lines], function(cs) flair_quick(cs, ".+"))
  text <- str_c(text, collapse = "<br>")
  return(text)
}
