#' Title
#'
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
