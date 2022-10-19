#' Add decorate chunk after chunk
#'
#' Call this function as an addin to add a decorate chunk after a selected
#' chunk.
#'
#' @export
chunk_addin <- function() {

  # Gets The active Documeent
  ctx <- rstudioapi::getActiveDocumentContext()

  # Checks that a document is active
  if (!is.null(ctx)) {

    # Extracts selection as a string
    selected_text <- ctx$selection[[1]]$text

    # modify string
    selected_text <- add_flair_chunk(selected_text)

    # replaces selection with string
    rstudioapi::modifyRange(ctx$selection[[1]]$range, selected_text)
  }
}

add_flair_chunk  <- function(x) {
  x <- stringr::str_split(x, "\n")[[1]]
  header_loc <- stringr::str_detect(x, "```\\{r")
  end_loc <- stringr::str_detect(x, "^```$")

  if (!any(header_loc) || !any(end_loc)) {
    stop("No chunk detected")
  }

  chunk_header <- x[header_loc][1]

  chunk_header <- stringr::str_remove(chunk_header, "```\\{r *,{0,1} *")

  chunk_header <- stringr::str_remove(chunk_header, "\\}")

  chunk_params <- stringr::str_split(chunk_header, ", *")[[1]]

  if (all(stringr::str_detect(chunk_params, "=")) || all(chunk_params == "")) {
    stop("Chunk must be named")
  }

  chunk_name <- chunk_params[!stringr::str_detect(chunk_params, "=")]

  flair_chunk <- c(
    '',
    glue::glue('```{r [chunk_name]_flair, echo = FALSE}',
               .open = "[", .close = "]"),
    glue::glue('decorate("{chunk_name}")'),
    '```'
  )

  chunk_params <- chunk_params[!stringr::str_detect(chunk_params, "include")]

  chunk_params <- c(chunk_params, "include = FALSE")

  x[which(header_loc)[1]] <- paste0("```{r ",
                                    paste(chunk_params, collapse = ", "),
                                    "}")

  res <- c(
    x[seq(1, which(end_loc)[1])],
    flair_chunk
  )

  if (which(end_loc)[1] < length(x)) {
    res <- c(res, x[seq(which(end_loc)[1] + 1, length(x))])
  }

  stringr::str_c(res, collapse = "\n")
}
