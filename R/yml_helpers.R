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
