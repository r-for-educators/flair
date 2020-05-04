.onLoad <- function(libname, pkgname) {
  op <- options()
  op.flair <- list(
    flair.knitr_defaults <- knitr::opts_chunk$get()
  )
  toset <- !(names(op.flair) %in% names(op))
  if(any(toset)) options(op.flair[toset])

  invisible()
}

