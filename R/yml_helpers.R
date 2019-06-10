#' Title
#'
#' @return
#' @export
#'
#' @examples
#' @rdname yml_blank
yml_blank <- function() {
  structure(list(), class = "yml_blank")
}


#' @export
#' @rdname yml_blank
is_yml_blank <- function(x) {
  inherits(x, "yml_blank")
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
yml_verbatim <- function(x) {
  structure(x, class = "verbatim")
}

yml_code <- function(x) {
  x <- rlang::enquo(x)
  glue::glue("`r {rlang::quo_text(x)} `")
}

#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
long_text <- function(...) {
  x <- c(...)

  #  treat vector elements as paragraph breaks
  x <- glue::glue_collapse(x, sep = "\n\n")

  #  `|` indicates to YAML that text can span line breaks
  #  just one space before `|`
  glue::glue_collapse(c("|", x), sep = "\n")
}

#' Title
#'
#' @param in_header
#' @param before_body
#' @param after_body
#'
#' @return
#' @export
#'
#' @examples
includes2 <- function(in_header = yml_blank(), before_body = yml_blank(), after_body = yml_blank()) {
  includes_list <- list(in_header = in_header, before_body = before_body, after_body = after_body)
  purrr::discard(includes_list, is_yml_blank)
}
