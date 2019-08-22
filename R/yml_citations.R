#' Set citation-related YAML options
#'
#' `yml_citations()` sets citation-related YAML fields, such as specifying a
#' bibliography file or style. For controlling the citation engine in PDF
#' documents, see the `citation_package` argument in
#' `rmarkdown::pdf_document()`.
#'
#' @template describe_yml_param
#' @param bibliography a path to a bibliography file, such as a .bib file
#' @param csl a path to a Citation Style Language (CSL) file. CSL files are used
#'   to specify the citation style; see the [CSL
#'   repository](https://github.com/citation-style-language/styles) for the CSL
#'   files of dozens of journals.
#' @param citation_abbreviations Path to a CSL abbreviations JSON file. See the
#'   [pandoc-citeproc
#'   documentation](http://manpages.ubuntu.com/manpages/xenial/man1/pandoc-citeproc.1.html).
#'    Note that the actual YAML field is `citation-abbreviations`.
#' @param link_citations Logical. Add citations hyperlinks to the corresponding
#'   bibliography entries?  Note that the actual YAML field is `link-citations`.
#' @param nocite Citation IDs (`"@item1"`) to include in the bibliography even if
#'   they are not cited in the document. Including the wildcard pattern `"@*"`
#'   will include all citations in the bibliography regardless of if they're
#'   cited in the document.
#' @param suppress_bibliography Logical. Suppress bibliography?
#'
#' @template describe_yml_output
#' @export
#'
#' @examples
#'
#' yml() %>%
#'   yml_citations(bibliography = "references.bib", csl = "aje.csl")
#'
#' @family citations
yml_citations <- function(
  .yml,
  bibliography = yml_blank(),
  csl = yml_blank(),
  citation_abbreviations = yml_blank(),
  link_citations = yml_blank(),
  nocite = yml_blank(),
  suppress_bibliography = yml_blank()
) {
  citation_opts <- list(
    bibliography = bibliography,
    csl = csl,
    "citation-abbreviations" = citation_abbreviations,
    "link-citations" = link_citations,
    nocite = nocite,
    "suppress-bibliography" = suppress_bibliography
  )

  citation_opts <- purrr::discard(citation_opts, is_yml_blank)

  warn_if_duplicate_fields(.yml, citation_opts)
  .yml[names(citation_opts)] <- citation_opts

  .yml
}

#' Write references as YAML fields
#'
#' `yml_reference()` creates YAML fields for references to be used in citation.
#' `reference()` is a simple function to add references to `yml_reference()`. The
#' easiest way to add references to an R Markdown file is to use a bibliography
#' file, such as .bib, in the `bibliography` field (see [yml_citations()]). For
#' documents with very few references, however, it might be useful to make the
#' references self-contained in the YAML. `yml_reference()` can also transform to
#' YAML `bibentry` and `citation` objects created by[bibentry()] and
#' [citation()]. To cite many R packages and convert the references to YAML,
#' it may be better to use [knitr::write_bib()] to write a bibliography file and
#' convert it with [`bib2yml()`].
#'
#' @template describe_yml_param
#' @param ... Fields relevant to the citation (e.g. bibtex fields)
#' @param .bibentry An object created by `bibentry()` or `citation()`. Note that
#'   this requires pandoc-citeproc to be installed.
#'
#' @template describe_yml_output
#' @export
#'
#' @examples
#'
#' ref <- reference(
#'   id = "fenner2012a",
#'   title = "One-click science marketing",
#'   author = list(
#'     family = "Fenner",
#'     given = "Martin"
#'   ),
#'   `container-title` = "Nature Materials",
#'   volume = 11L,
#'   URL = "http://dx.doi.org/10.1038/nmat3283",
#'   DOI = "10.1038/nmat3283",
#'   issue = 4L,
#'   publisher = "Nature Publishing Group",
#'   page = "261-263",
#'   type = "article-journal",
#'   issued = list(
#'     year = 2012,
#'     month = 3
#'   )
#' )
#'
#' yml() %>%
#'   yml_reference(ref)
#'
#' # from ?bibentry
#' bref <- c(
#'    bibentry(
#'      bibtype = "Manual",
#'      title = "boot: Bootstrap R (S-PLUS) Functions",
#'      author = c(
#'        person("Angelo", "Canty", role = "aut",
#'          comment = "S original"),
#'        person(c("Brian", "D."), "Ripley", role = c("aut", "trl", "cre"),
#'          comment = "R port, author of parallel support",
#'          email = "ripley@stats.ox.ac.uk")
#'      ),
#'      year = "2012",
#'      note = "R package version 1.3-4",
#'      url = "https://CRAN.R-project.org/package=boot",
#'      key = "boot-package"
#'    ),
#'
#'    bibentry(
#'      bibtype = "Book",
#'      title = "Bootstrap Methods and Their Applications",
#'      author = as.person("Anthony C. Davison [aut], David V. Hinkley [aut]"),
#'      year = "1997",
#'      publisher = "Cambridge University Press",
#'      address = "Cambridge",
#'      isbn = "0-521-57391-2",
#'      url = "http://statwww.epfl.ch/davison/BMA/",
#'      key = "boot-book"
#'    )
#' )
#' \dontrun{
#' # requires pandoc-citeproc to be installed
#' yml() %>%
#'   yml_reference(.bibentry = bref)
#'
#' yml() %>%
#'   yml_reference(.bibentry = citation("purrr"))
#'}
#'@family citations
yml_reference <- function(.yml, ..., .bibentry = NULL) {
  warn_if_duplicate_fields(.yml, list(reference = ""))

  if (!is.null(.bibentry)) {
    stop_if_not_type(.bibentry, "bibentry")
    # remove citation class if present
    if (inherits(.bibentry, "citation")) .bibentry <- as_bibentry(.bibentry)

    bib_yml <- bibentry2yml(.bibentry)

    .yml[names(bib_yml)] <- bib_yml
    return(.yml)
  }

  .yml$reference <- list(...)
  .yml
}

#' @export
#' @param id a character vector to use as the reference ID
#' @rdname yml_reference
reference <- function(id = NULL, ...) {
  stop_if_not_type(id, "character")
  list(
    id = id,
    ...
  )
}

bibentry2yml <- function(.bibentry) {
  on.exit(unlink_temporary_dir(), add = TRUE)
  .bibtex <- format(.bibentry, style = "Bibtex")
  writeLines(.bibtex, file.path(temporary_dir(), "bibtex.bib"))

  bib2yml(path = file.path(temporary_dir(), "bibtex.bib"))
}

as_bibentry <- function(x) {
  class(x) <- "bibentry"
  if (is.null(x$key)) {
    pkg_attr <- attr(x, "package")
    pkg_name <- ifelse(!is.null(pkg_attr), pkg_attr, "pkg")
    x$key <- glue::glue("R-{pkg_name}")
  }

  x
}

#' Convert bib files to YAML
#'
#' `bib2yml()` uses pandoc to convert a .bib file to YAML. It also accepts an
#' optional `yml` object to prepend to the the YAML from the .bib file. If you
#' want to cite several R packages, see [knitr::write_bib()] to write a
#' bibliography file and convert it with `bib2yml()`.
#'
#' @template describe_yml_param
#' @param path a path to the .bib file
#'
#' @template describe_yml_output
#' @export
#'
#' @family citations
bib2yml <- function(.yml = NULL, path) {
  args <- c(path, "--bib2yaml")

  bib_yml <- system2(pandoc_citeproc_exec(), args = args, stdout = TRUE) %>%
    paste(collapse = "\n") %>%
    as_yml()

  if (!is.null(.yml)) {
    warn_if_duplicate_fields(.yml, bib_yml)
    .yml[names(bib_yml)] <- bib_yml
    return(.yml)
  }

  bib_yml
}
