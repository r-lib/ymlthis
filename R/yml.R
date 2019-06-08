#' Title
#'
#' @param author
#' @param date
#'
#' @return
#' @export
#'
#' @examples
yml <- function(.yml = NULL, author = TRUE, date = TRUE) {
  if (is.null(.yml)) .yml <- list()
  .yml <- as_yml(.yml)

  if (author) .yml$author <- tryCatch(
    get_author_name(),
    error = function(e) "Author"
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

#' @export
print.yml <- function(x, ...) {
  #  save to be grabbed by last_yml()
  .ymlthis$.yml <- x
  yml_txt <- yaml::as.yaml(x, column.major = FALSE)

  cat("---\n")
  cat(yml_txt, ...)
  cat("---\n")

  invisible(x)
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
