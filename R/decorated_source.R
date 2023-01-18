as_decorated_source <- function(x, ...) {
  UseMethod("as_decorated_source", x)
}

#' @export
as_decorated_source.default <- function(x, ...) {
  stop("`as_decorated_source()` requires a character vector")
}

#' @export
as_decorated_source.character <- function(x, ...) {
  structure(x, class = "decorated_source")
}

is_decorated_source <- function(x) {
  if (length(x) > 1) {
    return(map_lgl(as.list(x), is_decorated_source))
  }
  inherits(x, "decorated_source")
}

modify_sources <- function(x, .f, ...) {
  # The function to apply to `decorated_source` items
  .f <- purrr::partial(purrr::as_mapper(.f), ...)

  # Ensure the `decorated_source` items retain their class post-processing
  .modify <- purrr::compose(as_decorated_source, .f)

  purrr::modify_if(x, is_decorated_source, .modify)
}
