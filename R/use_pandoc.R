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
use_pandoc_template <- function(type, path, source = c("rmarkdown", "pandoc")) {
  source <- match.arg(source)

  if (source == "rmarkdown") {
    x <- switch(
      type,
      latex = read_file("latex", "default.tex"),
      html = read_file("h", "default.html"),
      slidy = read_file("slidy", "default.html"),
      ioslides = read_file("ioslides", "default.html"),
      html_fragment = read_file("fragment", "default.html"),
      latex_fragment = read_file("fragment", "default.tex"),
    )
  }

  if (source == "pandoc") {
    template_file <- glue::glue(
      "https://raw.githubusercontent.com/jgm/pandoc-templates/master/default.{type}"
    )
    x <- readLines(template_file)
  }

  usethis::write_over(path, x)
}

read_file <- function(...) {
  readLines(
    system.file(
      "rmd",
      ...,
      package = "rmarkdown",
      mustWork = TRUE
    )
  )
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
