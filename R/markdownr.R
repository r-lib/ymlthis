#' Write code chunks programmatically
#'
#' `code_chunk()` assembles a knitr code chunk as a character vector.
#' `setup_chunk()` is a wrapper around `code_chunk()` to create setup chunks. By
#' default it uses `include = FALSE` and inserts `knitr::opts_chunk$set(echo =
#' TRUE)` into the chunk body. These are helper functions to write R Markdown
#' bodies for [use_rmarkdown()].
#'
#' @param chunk_code An expression. Surround with `{}` to capture multiple
#'   lines.
#' @param chunk_name The name of the chunk
#' @param chunk_args A `list` of chunk options
#'
#' @return a character vector
#' @export
#'
#' @examples
#' \donttest{
#'setup_chunk()
#'
#' code_chunk({
#'   yml() %>%
#'     yml_output(pdf_document())
#' }, chunk_name = "yml_example")
#' }
code_chunk <- function(chunk_code, chunk_name = NULL, chunk_args = NULL) {
  chunk_args <- splice_args(rlang::enquo(chunk_args))
  if (!is.null(chunk_name) && chunk_args != "") {
    chunk_name <- glue::glue("{chunk_name}, ")
  }

  chunk_code <- rlang::enexpr(chunk_code) %>%
    rlang::expr_text() %>%
    stringr::str_replace_all("^\\{\n|\\}$", "") %>%
    stringr::str_trim() %>%
    stringr::str_replace("\n$", "") %>%
    split_pluck() %>%
    stringr::str_replace("^\\s{4}", "") %>%
    glue::glue_collapse("\n")

  chunk_header <- paste0(
    "r",
    ifelse(is.null(chunk_name) && chunk_args == "", "", " "),
    chunk_name,
    chunk_args
  )

  glue::glue(
    "```{<<chunk_header>>}\n<<chunk_code>>\n```",
    .open = "<<",
    .close = ">>"
  )
}

#' @rdname code_chunk
#' @export
setup_chunk <- function(chunk_code = NULL, chunk_args = list(include = FALSE)) {
  arg_null <- rlang::enquo(chunk_code) %>%
    rlang::quo_is_null()

  if (arg_null) {
    code_chunk_txt <- code_chunk(
      chunk_code = {knitr::opts_chunk$set(echo = TRUE)},
      chunk_name = "setup",
      chunk_args = !!rlang::enquo(chunk_args)
    )
  } else {
    code_chunk_txt <- code_chunk(
      chunk_code = chunk_code,
      chunk_name = "setup",
      chunk_args = !!rlang::enquo(chunk_args)
    )
  }

  code_chunk_txt
}

splice_args <- function(x) {
  #  preserve calls without evaluating
  if (rlang::quo_is_null(x)) return("")
  args <- rlang::call_args(x)
  purrr::map2_chr(names(args), args, ~glue::glue("{.x} = {.y}")) %>%
    glue::glue_collapse(", ")
}
