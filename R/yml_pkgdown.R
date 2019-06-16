#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
pkgdown_template <- function(path = ".") {
  if (!requireNamespace("pkgdown", quietly = TRUE)) {
    stop("Installing `pkgdown` is required to use `pkgdown_template()`")
  }

  reference <- pkgdown::template_reference() %>%
    unclass()
  articles <- pkgdown::template_articles() %>%
    unclass()
  navbar <- pkgdown::template_navbar() %>%
    unclass()

  c(reference, articles, navbar) %>%
    as_yml()
}

#' Title
#'
#' * https://pkgdown.r-lib.org/articles/pkgdown.html
#' * ?pkgdown::build_reference()
#'
#' @param .yml
#' @param ...
#' @param title
#' @param desc
#' @param contents
#' @param exclude
#'
#' @return
#' @export
#'
#' @examples
yml_pkgdown_reference <- function(.yml, ...) {
  .yml$references <- c(...)
  .yml
}

#' @export
#' @rdname yml_pkgdown_reference
pkgdown_ref <- function(
  title = yml_blank(),
  desc = yml_blank(),
  contents = yml_blank(),
  exclude = yml_blank()
) {
  list(
    title = title,
    desc = desc,
    contents = contents,
    exclude = exclude
  ) %>%
    purrr::discard(is_yml_blank)
}

#' Title
#'
#' * https://pkgdown.r-lib.org/articles/pkgdown.html
#' * ?pkgdown::build_articles()
#'
#' @param .yml
#' @param as_is
#' @param extension
#'
#' @return
#' @export
#'
#' @examples
yml_pkgdown <- function(.yml, as_is = yml_blank(), extension = yml_blank()) {
  .yml$pkgdown <- list(as_is = as_is, extension = extension)
  .yml
}


#' Title
#'
#' * https://pkgdown.r-lib.org/articles/pkgdown.html
#'
#' @param .yml
#' @param one_page
#'
#' @return
#' @export
#'
#' @examples
yml_pkgdown_news <- function(.yml, one_page = yml_blank()) {
  .yml$news <- list(one_page = one_page)
  .yml
}


#' Title
#'
#' * https://pkgdown.r-lib.org/articles/pkgdown.html
#' * ?pkgdown::build_articles
#'
#' @param .yml
#' @param ...
#' @param title
#' @param desc
#' @param exclude
#'
#' @return
#' @export
#'
#' @examples
yml_pkgdown_articles <- function(.yml, ...) {
  .yml$articles <- c(...)

  .yml
}

#' @export
#' @rdname yml_pkgdown_articles
pkgdown_article <- function(
  title = yml_blank(),
  desc = yml_blank(),
  contents = yml_blank()
) {
  list(
    title = title,
    desc = desc,
    contents = contents
  ) %>%
    purrr::discard(is_yml_blank)
}


#' Title
#'
#' * ?pkgdown::build_reference
#'
#' @param .yml
#' @param figures
#' @param dev
#' @param dpi
#' @param dev.args
#' @param fig.ext
#' @param fig.width
#' @param fig.height
#' @param fig.retina
#' @param fig.asp
#'
#' @return
#' @export
#'
#' @examples
yml_pkgdown_figures <- function(
  .yml,
  figures = yml_blank(),
  dev = yml_blank(),
  dpi = yml_blank(),
  dev.args = yml_blank(),
  fig.ext = yml_blank(),
  fig.width = yml_blank(),
  fig.height = yml_blank(),
  fig.retina = yml_blank(),
  fig.asp = yml_blank()
) {
  .yml$figures <- list(
    dev = dev,
    dpi = dpi,
    dev.args = dev.args,
    fig.ext = fig.ext,
    fig.width = fig.width,
    fig.height = fig.height,
    fig.retina = fig.retina,
    fig.asp = fig.asp
  ) %>%
    purrr::discard(is_yml_blank)

  .yml
}

yml_pkgdown_url <- function(url) {

}

#' Title
#'
#' @param .yml
#' @param api_key
#' @param index_name
#'
#' @return
#' @export
#'
#' @examples
yml_pkgdown_docsearch <- function(.yml, api_key, index_name) {
  docsearch <- list(
    template = list(
      params = list(
        docsearch = list(
          api_key = api_key,
          index_name = index_name
        )
      )
    )
  )

  .yml[names(docsearch)] <- docsearch

  .yml
}

