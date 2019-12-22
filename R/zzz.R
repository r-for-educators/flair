# Sets hook options upon library load

.onLoad <- function(libname, pkgname) {

  knitr::knit_hooks$set(chunk = function(x, options) {
    if (!is.null(options$demo)) {
      assign(options$demo, x, envir = .GlobalEnv)
    }
      x
  })

  invisible()
}


