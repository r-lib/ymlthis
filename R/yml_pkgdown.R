#' Generate a full YAML template for your pkgdown site
#'
#' pkgdown includes three helpful `pkgdown::template_*()` functions to generate
#' the navbar, reference, and article YAML for the `_pkgdown.yml` file.
#' `pkgdown_template()` is a wrapper function that runs all three, combines
#' them, and converts them to a `yml` object. You may also pass
#' `pkgdown::template_*()` functions to `as_yml()` to convert the individual
#' sections. `pkgdown_template()` is particularly useful with
#' `use_pkgdown_yml()` to write directly to the `_pkgdown.yml` file.
#'
#' @param path The path to your package directory
#'
#' @template describe_yml_output
#' @export
#'
#' @examples
#' \dontrun{
#' pkgdown_template() %>%
#'   use_pkgdown_yml()
#' }
#'
#' @family pkgdown
#' @seealso [`use_pkgdown_yml()`]
pkgdown_template <- function(path = ".") {
  if (!requireNamespace("pkgdown", quietly = TRUE)) {
    stop("Installing `pkgdown` is required to use `pkgdown_template()`")
  }

  reference <- pkgdown::template_reference(path = path)
  articles <- pkgdown::template_articles(path = path)
  navbar <- pkgdown::template_navbar(path = path)

  c(reference, articles, navbar) %>%
    as_yml()
}


#' Set Top-level YAML options for pkgdown
#'
#' These functions set YAML for various pkgdown options to be used in
#' `_pkgdown.yml`. The options are described in greater depth in the [pkgdown
#' vignette](https://pkgdown.r-lib.org/articles/pkgdown.html) and in the help
#' pages for `pkgdown::build_articles()` and `pkgdown::build_reference()`.
#' Essentially, they control the build of vignettes and function references.
#' pkgdown also uses the same approach to navbars as R Markdown.
#' [`yml_navbar()`] and friends will help you write the YAML for that. A useful
#' approach to writing pkgdown YAML might be to use `pkgdown_template()` to
#' build a template based on your package directory, modify with these functions
#' or [yml_replace()] and [yml_discard()], then pass the results to
#' [use_pkgdown_yml()] to write to `_pkgdown.yml`
#'
#'
#' @template describe_yml_param
#' @template describe_dots_param
#' @param as_is Logical. Use the output_format and options that you have
#'   specified.
#' @param extension Specify the output extension, e.g. "pdf"
#'
#' @template describe_yml_output
#' @export
#'
#' @examples
#'
#' yml(author = FALSE, date = FALSE) %>%
#'   yml_pkgdown(
#'     as_is = TRUE,
#'     extension = "pdf"
#'   ) %>%
#'   yml_pkgdown_reference(
#'     pkgdown_ref(
#'       title = "pkgdown functions",
#'       contents = "contains('function_name')"
#'     )
#'   ) %>%
#'   yml_pkgdown_articles(
#'     pkgdown_article(
#'       title = "Introduction to the package"
#'     )
#'   )
#'
#' @family pkgdown
#' @family websites
#' @seealso [use_pkgdown_yml()] [yml_navbar()]
yml_pkgdown <- function(.yml, as_is = yml_blank(), extension = yml_blank()) {
  .yml$pkgdown <- list(as_is = as_is, extension = extension)
  .yml
}


#' @param title The title of the article or reference
#' @param desc A description of the article or reference
#' @param contents The contents, which can also be dplyr-style tidy selectors
#'   (e.g "contains('index')").
#' @param exclude What to exclude of captured by `contents`
#'
#' @export
#' @rdname yml_pkgdown
yml_pkgdown_reference <- function(.yml, ...) {
  warn_if_duplicate_fields(.yml, list(references = ""))
  .yml$references <- c(...)
  .yml
}

#' @export
#' @rdname yml_pkgdown
pkgdown_ref <- function(
  title = yml_blank(),
  desc = yml_blank(),
  contents = yml_blank(),
  exclude = yml_blank(),
  ...
) {
  list(
    title = title,
    desc = desc,
    contents = contents,
    exclude = exclude,
    ...
  ) %>%
    purrr::discard(is_yml_blank)
}


#' @param one_page Logical. Create one page per release for NEWS.md?
#' @export
#' @rdname yml_pkgdown
yml_pkgdown_news <- function(.yml, one_page = yml_blank()) {
  warn_if_duplicate_fields(.yml, list(news = ""))
  .yml$news <- list(one_page = one_page)
  .yml
}


#' @export
#' @rdname yml_pkgdown
yml_pkgdown_articles <- function(.yml, ...) {
  warn_if_duplicate_fields(.yml, list(articles = ""))
  .yml$articles <- c(...)

  .yml
}

#' @export
#' @rdname yml_pkgdown
pkgdown_article <- function(
  title = yml_blank(),
  desc = yml_blank(),
  contents = yml_blank(),
  exclude = yml_blank(),
  ...
) {
  list(
    title = title,
    desc = desc,
    contents = contents,
    exclude = exclude,
    ...
  ) %>%
    purrr::discard(is_yml_blank)
}


#' @param dev The graphics device (default: "grDevices::png")
#' @param dpi The DPI (default: 96)
#' @param dev.args A vector of arguments to pass to `dev`
#' @param fig.ext The figure extension (default: "png")
#' @param fig.width The figure width (default: 7.2916667)
#' @param fig.height The figure height (default: `NULL`)
#' @param fig.retina The figure retina value (default: 2)
#' @param fig.asp The aspect ratio (default: 1.618)
#' @export
#' @rdname yml_pkgdown
yml_pkgdown_figures <- function(
  .yml,
  dev = yml_blank(),
  dpi = yml_blank(),
  dev.args = yml_blank(),
  fig.ext = yml_blank(),
  fig.width = yml_blank(),
  fig.height = yml_blank(),
  fig.retina = yml_blank(),
  fig.asp = yml_blank(),
  ...
) {
  warn_if_duplicate_fields(.yml, list(figures = ""))
  .yml$figures <- list(
    dev = dev,
    dpi = dpi,
    dev.args = dev.args,
    fig.ext = fig.ext,
    fig.width = fig.width,
    fig.height = fig.height,
    fig.retina = fig.retina,
    fig.asp = fig.asp,
    ...
  ) %>%
    purrr::discard(is_yml_blank)

  .yml
}


#' @param api_key The API key provided by docsearch (see the [pkgdown
#'   vignette](https://pkgdown.r-lib.org/articles/pkgdown.html))
#' @param index_name The index name provided by docsearch (see the [pkgdown
#'   vignette](https://pkgdown.r-lib.org/articles/pkgdown.html))
#' @param url the URL specifying the location of your documentation
#' @export
#' @rdname yml_pkgdown
yml_pkgdown_docsearch <- function(.yml, api_key = yml_blank(), index_name = yml_blank(), url = yml_blank()) {
  docsearch <- list(
    template = list(
      params = list(
        docsearch = list(
          api_key = api_key,
          index_name = index_name
        ) %>%
          purrr::discard(is_yml_blank)
      )
    ),
    url = url
  ) %>%
    purrr::discard(is_yml_blank)

  warn_if_duplicate_fields(.yml, docsearch)
  .yml[names(docsearch)] <- docsearch

  .yml
}

