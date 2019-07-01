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
#' @param .yml
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
yml_reference <- function(.yml, ...) {
  warn_if_duplicate_fields(.yml, list(reference = ""))
  .yml$reference <- list(...)
  .yml
}

#' @export
#' @rdname yml_reference
reference <- function(id, ...) {
  stop_if_not_type(id, "character")
  list(
    id = id,
    ...
  )
}
