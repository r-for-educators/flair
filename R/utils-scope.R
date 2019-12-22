#' Runs and prints from string, in parent environment
#'
#' Shortcut function to rescope a code string and then run and print output.  Looks for object assignments of the form \code{foo <-} and rescopes to \code{foo <<-}, then evaluates code string.
#'
#' @param .code_string A string containing runnable R code.
#'
#' @return Nothing; side effects from \code{print()} only.
#'
scope_run_print <- function(.code_string) {


  # For object definitions, rescope but print nothing.
  if (stringr::str_detect(.code_string, "^[^\\s]+\\s*\\<\\-")) {

    .code_string %>%
      str_replace("(?!=\\<)\\<\\-", "<<-") %>%
      parse(text = .) %>%
      eval()

  } else {

    .code_string %>%
      parse(text = .) %>%
      eval() %>%
      print()

  }

}

#' Runs code from string, in parent environment
#'
#' Shortcut function to rescope a code string and then run (but not print).
#' Looks for object assignments of the form \code{foo <-} and rescopes to
#' \code{foo <<-}, then evaluates code string.
#'
#' @param .code_string A string containing runnable R code.
#'
#' @return Nothing; side effects in environment only.
#'
scope_and_run <- function(.code_string) {


  # Rescope and run if an assignment is involved.
  if (stringr::str_detect(.code_string, "^[^\\s]+\\s*\\<\\-")) {

    .code_string %>%
      str_replace("(?!=\\<)\\<\\-", "<<-") %>%
      parse(text = .) %>%
      eval()

  }

}

