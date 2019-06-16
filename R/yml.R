#' Create a new yml object
#'
#' @param .yml a character vector, `yml` object, or YAML-like list. See details.
#' @param get_yml logical. Use YAML stored in `getOption("ymlthis.default_option")`?
#' @param author logical. Get default author name?
#' @param date logical. Get default date?
#' @param yml_handlers a `list` containing functions to handle YAML printing. See details.
#'
#' @return
#' @export
#'
#' @examples
yml <- function(.yml = NULL, get_yml = FALSE, author = TRUE, date = TRUE, yml_handlers = NULL) {
  if (is.null(.yml)) .yml <- list()
  .yml <- as_yml(.yml)

  if (get_yml) default_yml <- get_yml_defaults()
  if (get_yml && !is.null(default_yml)) {
    default_fields <- names(default_yml)
    if ("author" %in% default_fields) author <- FALSE
    if ("date" %in% default_fields) date <- FALSE
    .yml[default_fields] <- default_yml
  }

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


#' @export
as_yml.list <- function(x, ...) {
  structure(
    x,
    class = "yml"
  )
}

#' @export
as_yml.character <- function(x, ...) {
  .yml <- yaml::yaml.load(x)
  as_yml(.yml)
}

yml_handlers <- function() {
  list(
    NULL = function(x) yml_verbatim("null"),
    glue = function(x) as.character(x),
    Date = function(x) as.character(x),
    logical = function(x) yml_verbatim(ifelse(x, "true", "false"))
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
    #  color list hyphens and single colons silver
    stringr::str_replace_all("-\\s", crayon::silver) %>%
    stringr::str_replace_all("(?<!\\:)\\:(?!\\:)", crayon::silver) %>%
    #  color fields green
    stringr::str_split("\n") %>%
    purrr::pluck(1) %>%
    stringr::str_replace(field_names, crayon::green) %>%
    paste(collapse = "\n")

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
