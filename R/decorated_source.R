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
  # `.f()` is the function we'll apply to any `decorated_source` items in `x`.
  # `as_mapper()` lets us use the same syntax as `purrr::map()`, i.e. function
  # names, anonymous functions, `~ .x` style, etc. The `purrr::partial()` call
  # fills in the arguments of the function `.f` with any arguments in the `...`
  # -- it's like we've updated the default values of the arguments of `.f`.
  .f <- purrr::partial(purrr::as_mapper(.f), ...)

  # Ensure the `decorated_source` items retain their class after we apply `.f()`
  .f_decorated <- function(item) {
    modified <- .f(item)
    as_decorated_source(modified)
  }

  purrr::modify_if(x, is_decorated_source, .f_decorated)
}
