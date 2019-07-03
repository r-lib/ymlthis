#' Title
#'
#' @param .yml
#' @param bibliography
#' @param csl
#' @param citation_abbreviations
#' @param natbib
#' @param biblatex
#'
#' @return
#' @export
#'
#' @examples
yml_citations <- function(
  .yml,
  bibliography = yml_blank(),
  csl = yml_blank(),
  citation_abbreviations = yml_blank(),
  natbib = yml_blank(),
  biblatex = yml_blank()
) {
  citation_opts <- list(
    bibliography = bibliography,
    csl = csl,
    "citation-abbreviations" = citation_abbreviations,
    natbib = natbib,
    biblatex = biblatex
  )

  citation_opts <- purrr::discard(citation_opts, is_yml_blank)

  warn_if_duplicate_fields(.yml, citation_opts)
  .yml[names(citation_opts)] <- citation_opts

  .yml
}

#' Title
#'
#' See ?bibentry
#'
#' @param .yml
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
yml_reference <- function(.yml, ..., .bibentry = NULL) {
  warn_if_duplicate_fields(.yml, list(reference = ""))

  if (!is.null(.bibentry)) {
    stop_if_not_type(.bibentry, "bibentry")
    # remove citation class if present
    if (inherits(.bibentry, "citation")) .bibentry <- as_bibentry(.bibentry)

    on.exit(unlink(tempdir()))
    .bibtex <- capture.output(print(.bibentry, style = "Bibtex"))
    writeLines(.bibtex, file.path(tempdir(), "bibtex.bib"))
    bib_yml <- bib2yml(path = file.path(tempdir(), "bibtex.bib"))

    .yml[names(bib_yml)] <- bib_yml
    return(.yml)
  }

  .yml$reference <- list(...)
  .yml
}

#' @export
#' @rdname yml_reference
reference <- function(id = NULL, ...) {
  stop_if_not_type(id, "character")
  list(
    id = id,
    ...
  )
}

as_bibentry <- function(x) {
  class(x) <- "bibentry"
  if (is.null(x$key)) {
    pkg_attr <- attr(x, "package")
    pkg_name <- ifelse(!is.null(pkg_attr), pkg_attr, "pkg")
    x$key <- glue::glue("{tolower(x$author$family[1])}-{pkg_name}-{x$year}")
  }

  x
}

#' Convert bib files to YAML
#'
#' use knitr example to write package bib
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
bib2yml <- function(.yml = NULL, path) {
  bib_yml <- glue::glue("pandoc-citeproc {path} --bib2yaml") %>%
    system(intern = TRUE) %>%
    paste(collapse = "\n") %>%
    as_yml()

  if (!is.null(.yml)) {
    warn_if_duplicate_fields(.yml, bib_yml)
    .yml[names(bib_yml)] <- bib_yml
    return(.yml)
  }

  bib_yml
}
