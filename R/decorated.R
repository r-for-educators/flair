new_decorated <- function(knitted, code = NULL, label = NULL) {
  # convert knitted string to a list with sources separate from output
  knitted <- knitted %>% src_to_list()

  structure(
    knitted,
    class = "decorated",
    orig_code_text = code,
    chunk_name = label
  )
}

as_decorated <- function(x, ...) UseMethod("as_decorated", x)

#' @export
as_decorated.default <- function(x, ...) {
  stop("as_decorated() requires a list input and does not perform validation.")
}

#' @export
as_decorated.list <- function(x, ...) {
  class(x) <- "decorated"
  x
}
