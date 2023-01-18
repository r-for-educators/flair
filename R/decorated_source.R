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

#' @export
is.decorated_source <- function(x) {
  inherits(x, "decorated_sour")
}
