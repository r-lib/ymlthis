#' Use pandoc templates and custom highlight themes
#'
#' Pandoc has several built in templates and code highlighting themes that can
#' be customized and included in the `template` and `highlight-style` YAML
#' fields, respectively. `pandoc_template_types()` and
#' `pandoc_highlight_styles()` return the available templates and highlight
#' styles in pandoc, respectively. `use_pandoc_template()` creates a new file
#' based on a template from pandoc or R Markdown and
#' `use_pandoc_highlight_style()` creates a new highlight theme file based on an
#' existing pandoc theme.
#'
#' @param theme The name of the theme
#' @param type The template type
#' @param source The template source ("pandoc" or "rmarkdown")
#' @param path The path to write the file to
#'
#' @return a character vector
#' @export
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
  stop_if_pandoc_not_installed()
  system2(rmarkdown::pandoc_exec(), "--list-highlight-styles", stdout = TRUE)
}


#' @export
#' @rdname pandoc_template_types
use_pandoc_template <- function(type, path, source = c("rmarkdown", "pandoc")) {
  stop_if_pandoc_not_installed()
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
    args <- glue::glue("--print-default-template={type}")
    x <- system2(rmarkdown::pandoc_exec(), args = args, stdout = TRUE)
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


#' @export
#' @rdname pandoc_template_types
use_pandoc_highlight_style <- function(theme, path) {
  stop_if_pandoc_not_installed()
  if (!grepl("\\.theme$", path)) {
    stop("`path` must end in `.theme`", call. = FALSE)
  }

  args <- glue::glue("--print-highlight-style={theme}")
  x <- system2(rmarkdown::pandoc_exec(), args = args, stdout = TRUE)

  usethis::write_over(path, x)
}

stop_if_pandoc_not_installed <- function() {
  stop_if_rmarkdown_not_installed()
  if (!rmarkdown::pandoc_available()) {
    stop("pandoc must be installed to use this function")
  }
}

pandoc_citeproc_exec <- function() {
  citeproc_path <- paste0(rmarkdown::pandoc_exec(), "-citeproc")
  if (file.exists(citeproc_path)) return(citeproc_path)

  "pandoc-citeproc"
}

