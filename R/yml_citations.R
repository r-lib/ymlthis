yml_citations <- function(
  bibliography = NULL,
  csl = NULL,
  citation_abbreviations = NULL,
  natbib = NULL,
  biblatex = NULL
) {
  list(
    bibliography = bibliography,
    csl = csl,
    "citation-abbreviations" = citation_abbreviations,
    natbib = natbib,
    biblatex = biblatex
  )
}
