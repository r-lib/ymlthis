#' Set YAML related to rticles output formats
#'
#' The rticles package include numerous output formats specific to academic
#' journals. All of these can take YAML similar to `pdf_document()`.
#' Additionally, two templates include custom YAML, `rticles::sage_article()`
#' and `rticles::sim_article()`. See the help pages for these functions for more
#' details and the sources of the LaTeX templates used for each.
#'
#' @template describe_yml_param
#' @param title Title of the manuscript
#' @param runninghead A character vector, a short author list for the header
#'   (sage_article)
#' @param author A list of authors, containing `name` and `num` fields
#'   (sage_article, sim_article). Use `rticles_author()` or a list to specify.
#' @param authormark A character vector, the short author list for the header
#'   (sim_article)
#' @param address list containing `num` and `org` for defining author
#'   affiliations (sage_article, sim_article). Use `rticles_address()` or a list
#'   to specify.
#' @param corrauth corresponding author `name` and `address` (sage_article). Use
#'   `rticles_corr_author()` or a list to specify.
#' @param corres `author` and `address` for correspondence (sim_article). Use
#'   `rticles_corr_author()` or a list to specify.
#' @param email The email of the correspondence author (sage_article)
#' @param abstract The abstract, limited to 200 words (sage_article), 250 words
#'   (sim_article)
#' @param received,revised,accepted The dates of submission, revision, and
#'   acceptance of the manuscript (sim_article)
#' @param keywords The keywords for the article (sage_article), up to 6 keywords
#'   (sim_article)
#' @param bibliography BibTeX `.bib` file name  (sage_article, sim_article)
#' @param longtable Logical. Include the longtable package? Used by default from
#'   pandoc to convert markdown to LaTeX code (sim_article)
#' @param classoption a character vector of `classoption` options for the
#'   `sagej` class (sage_article)
#' @param header_includes additional LaTeX code to include in the header, before
#'   the `\\begin\{document\}` statement (sage_article, sim_article). Note that
#'   the actual YAML field is `header-includes`
#' @param include_after additional LaTeX code to include before the
#'   `\\end\{document\}` statement (sage_article, sim_article). Note that the
#'   actual YAML field is `include-after`.
#' @template describe_dots_param
#'
#' @template describe_yml_output
#' @export
#'
#' @examples
#'
#' yml() %>%
#'   yml_rticles_opts(received = "09-12-2014")
#'
yml_rticles_opts <- function(
  .yml,
  title = yml_blank(),
  runninghead = yml_blank(),
  author = yml_blank(),
  authormark = yml_blank(),
  address = yml_blank(),
  corrauth = yml_blank(),
  corres = yml_blank(),
  email = yml_blank(),
  abstract = yml_blank(),
  received = yml_blank(),
  revised = yml_blank(),
  accepted = yml_blank(),
  keywords = yml_blank(),
  bibliography = yml_blank(),
  longtable = yml_blank(),
  classoption = yml_blank(),
  header_includes = yml_blank(),
  include_after = yml_blank(),
  ...
) {
  rticles_opts <- list(
    title = title,
    runninghead = runninghead,
    author = author,
    address = address,
    authormark = authormark,
    corrauth = corrauth,
    corres = corres,
    email = email,
    abstract = abstract,
    received = received,
    revised = revised,
    accepted = accepted,
    keywords = keywords,
    bibliography = bibliography,
    longtable = longtable,
    classoption = classoption,
    "header-includes" = header_includes,
    "include-after" = include_after,
    ...
  )

  rticles_opts <- purrr::discard(rticles_opts, is_yml_blank)

  warn_if_duplicate_fields(.yml, rticles_opts)
  .yml[names(rticles_opts)] <- rticles_opts

  .yml
}

#' @param name The author's name
#' @param num The author's number or address number
#'
#' @export
#' @rdname yml_rticles_opts
rticles_author <- function(name = yml_blank(), num = yml_blank()) {
  list(
    name = name,
    num = num
  ) %>%
    purrr::discard(is_yml_blank)
}

#' @param org The author's organization
#'
#' @export
#' @rdname yml_rticles_opts
rticles_address <- function(name = yml_blank(), org = yml_blank()) {
  list(
    name = name,
    org = org
  ) %>%
    purrr::discard(is_yml_blank)
}


#' @export
#' @rdname yml_rticles_opts
rticles_corr_author <- function(name = yml_blank(), author = yml_blank(), address = yml_blank()) {
  list(
    name = name,
    author = author,
    address = address
  ) %>%
    purrr::discard(is_yml_blank)
}
