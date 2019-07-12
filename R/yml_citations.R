#' Set citation-related YAML options
#'
#' `yml_citations()` sets citation-related YAML fields, such as specifying a
#' bibligraphy file or style.
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
#' @param link_citations Logical. Add citations hyperlinks to the corresponding
#'   bibliography entries?
#' @param natbib Use natbib for citations in LaTeX output. This option is not
#'   for use with the pandoc-citeproc filter or with PDF output. It is intended
#'   for use in producing a LaTeX file that can be processed with bibtex.
#' @param biblatex Use biblatex for citations in LaTeX output. This option is
#'   not for use with the pandoc-citeproc filter or with PDF output. It is
#'   intended for use in producing a LaTeX file that can be processed with
#'   bibtex or biber.
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
  natbib = yml_blank(),
  biblatex = yml_blank()
) {
  citation_opts <- list(
    bibliography = bibliography,
    csl = csl,
    "citation-abbreviations" = citation_abbreviations,
    "link-citations" = link_citations,
    natbib = natbib,
    biblatex = biblatex
  )

  citation_opts <- purrr::discard(citation_opts, is_yml_blank)

  warn_if_duplicate_fields(.yml, citation_opts)
  .yml[names(citation_opts)] <- citation_opts

  .yml
}

#' Write references as YAML fields
#'
#' `yml_reference()` creates YAML fields for references to be used in citation.
#' `reference()` is a simple function to add references. The easiest way to add
#' references to a R Markdown file is to use a bibliography file, such as .bib,
#' in the `bibliography` field (see [yml_citations()]). For documents with very
#' few references, however, it might be useful to make the references
#' self-contained in the YAML. `yml_reference()` can also transform `bibentry`
#' and `citation` objects to YAML. See [bibentry()] and [citation()]. To cite
#' several R packages and convert the references to YAML, it may be better to
#' use [knitr::write_bib()] to write a bibliography file and convert it with
#' `[bib2yml()]`.
#'
#' @template describe_yml_param
#' @param ... Fields relevant to the citation (e.g. bibtex fields)
#' @param .bibentry an object created by `bibentry()` or `citation()`
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
#'
#' yml() %>%
#'   yml_reference(.bibentry = bref)
#'
#' yml() %>%
#'   yml_reference(.bibentry = citation("purrr"))
#'
#' @family citations
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
  on.exit(unlink(tempdir()))
  .bibtex <- utils::capture.output(print(.bibentry, style = "Bibtex"))
  writeLines(.bibtex, file.path(tempdir(), "bibtex.bib"))

  bib2yml(path = file.path(tempdir(), "bibtex.bib"))
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
#' option `yml` object to prepend to the the YAML from the .bib file. If you
#' want to cite several R packages, se [knitr::write_bib()] to write a
#' bibliography file and convert it with `[bib2yml()]`.
#'
#' @template describe_yml_param
#' @param path a path to the .bib file
#'
#' @template describe_yml_output
#' @export
#'
#' @family citations
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
