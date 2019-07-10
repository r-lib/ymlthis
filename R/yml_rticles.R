#' Title
#'
#' * ?rticles::acm_article
#' * need to indicate which options belong to which functions
#'
#' @param title
#' @param runninghead
#' @param author
#' @param authormark
#' @param address
#' @param corrauth
#' @param corres
#' @param email
#' @param abstract
#' @param received
#' @param revised
#' @param accepted
#' @param keywords
#' @param bibliography
#' @param longtable
#' @param classoption
#' @param header_includes
#' @param include_after
#' @param ... additional named R objects, such as characters or lists, to
#'   transform into YAML
#'
#' @return
#' @export
#'
#' @examples
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
