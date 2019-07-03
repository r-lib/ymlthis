#' Title
#'
#' @return
#' @export
#'
#' @examples
pandoc_template_types <- function() {
  c(
    "asciidoc",
    "asciidoctor",
    "commonmark",
    "context",
    "docbook4",
    "docbook5",
    "dokuwiki",
    "dzslides",
    "epub2",
    "epub3",
    "haddock",
    "html4",
    "html5",
    "icml",
    "jats",
    "jira",
    "latex",
    "latex.orig",
    "latex.rej",
    "man",
    "markdown",
    "mediawiki",
    "ms",
    "muse",
    "opendocument",
    "opml",
    "org",
    "plain",
    "revealjs",
    "rst",
    "rtf",
    "s5",
    "slideous",
    "slidy",
    "tei",
    "texinfo",
    "textile",
    "xwiki",
    "zimwiki"
  )
}

#' @export
#' @rdname pandoc_template_types
pandoc_highlight_styles <- function() {
  system("pandoc --list-highlight-styles", intern = TRUE)
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
    x <- glue::glue("pandoc --print-default-template={type}") %>%
    system(intern = TRUE)
    # template_file <- glue::glue(
    #   "https://raw.githubusercontent.com/jgm/pandoc-templates/master/default.{type}"
    # )
    # x <- readLines(template_file)
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
use_pandoc_highlight_style <- function(theme, path) {
  if (!grepl("//.theme$", path)) {
    stop("`path` must end in `.theme`", call. = FALSE)
  }

  x <- glue::glue("pandoc --print-highlight-style {theme}") %>%
    system(intern = TRUE)

  usethis::write_over(path, x)
}
