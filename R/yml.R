#' Title
#'
#' @param author
#' @param date
#'
#' @return
#' @export
#'
#' @examples
yml <- function(.yml = NULL, author = TRUE, date = TRUE, yml_handlers = NULL) {
  if (is.null(.yml)) .yml <- list()
  .yml <- as_yml(.yml)

  if (author) .yml$author <- tryCatch(
    get_author_name(),
    error = function(e) yml_blank()
  )
  if (date) .yml$date <- format_sys_date()

  .yml
}

#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
as_yml <- function(x, ...) {
  UseMethod("as_yml")
}

as_yml.list <- function(x, ...) {
  structure(
    x,
    class = "yml"
  )
}

as_yml.character <- function(x, ...) {
  .yml <- yaml::yaml.load(x)
  as_yml(.yml)
}

yml_handlers <- function() {
  list(
    NULL = function(x) yml_verbatim("null"),
    glue = function(x) as.character(x),
    Date = function(x) as.character(x)
  )
}

#' @export
print.yml <- function(x, ...) {
  #  save to be grabbed by last_yml()
  .ymlthis$.yml <- x
  yml_txt <- yaml::as.yaml(
    x,
    handlers = yml_handlers(),
    column.major = FALSE
  )

  field_names <- seq(from = 0, to = purrr::vec_depth(x) - 1) %>%
    purrr::map(~purrr::map_depth(x, .x, names, .ragged = TRUE)) %>%
    unlist(use.names = FALSE) %>%
    paste(collapse = "|")

  yml_txt <- yml_txt %>%
    #  color list hyphens silver
    stringr::str_replace_all("-\\s", crayon::silver) %>%
    #  color fields green
    stringr::str_replace_all(field_names, crayon::green)

  cat_silver("---\n")
  cat(yml_txt, ...)
  cat_silver("---\n")

  invisible(x)
}

cat_silver <- function(x) {
  cat(crayon::silver(x))
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
is_yml <- function(x) inherits(x, "yml")

#  set environment to store last yml
.ymlthis <- new.env(parent = emptyenv())
.ymlthis$.yml <- list()

#' Title
#'
#' @return
#' @export
#'
#' @examples
last_yml <- function() {
  if (rlang::is_empty(.ymlthis$.yml)) .ymlthis$.yml <- yml()
  .ymlthis$.yml
}
