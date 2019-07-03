#' Title
#'
#' @return
#' @export
#'
#' @examples
pandoc_template_types <- function() {
  templates <- gh::gh("/repos/jgm/pandoc-templates/contents") %>%
    purrr::map_chr("name")

  templates[!grepl("^README", templates)] %>%
    stringr::str_remove_all("default\\.")
}

#' Title
#'
#' @param type
#' @param path
#'
#' @return
#' @export
#'
#' @examples
use_pandoc_template <- function(type, path) {
  x <- glue::glue(
    "https://raw.githubusercontent.com/jgm/pandoc-templates/master/default.{type}"
  ) %>%
    readLines()

  usethis::write_over(path, x)
}

#' Title
#'
#' @param theme
#' @param path
#'
#' @return
#' @export
#'
#' @examples
use_pandoc_highlight_theme <- function(theme, path) {
  if (!grepl("//.theme$", path)) {
    stop("`path` must end in `.theme`", call. = FALSE)
  }

  x <- glue::glue("pandoc --print-highlight-style {theme}") %>%
    system(intern = TRUE)

  usethis::write_over(path, x)
}
