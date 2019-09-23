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
#' \donttest{
#' # requires this to be a package directory
#' pkgdown_template() %>%
#'   use_pkgdown_yml()
#' }
#'
#' @family pkgdown
#' @seealso [`use_pkgdown_yml()`]
pkgdown_template <- function(path = ".") {
  stop_if_not_installed("pkgdown")

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
#' pages for `pkgdown::build_site()`, `pkgdown::build_articles()`, `pkgdown::build_reference()`, and `pkgdown::build_tutorials()`.
#' Essentially, they control the build of vignettes and function references.
#' pkgdown also uses the same approach to navbars as R Markdown.
#' [`yml_navbar()`] and friends will help you write the YAML for that. A useful
#' approach to writing pkgdown YAML might be to use `pkgdown_template()` to
#' build a template based on your package directory, modify with
#' `yml_pkgdown_*()` and `pkgdown_*()` functions or [yml_replace()] and
#' [yml_discard()], then pass the results to [use_pkgdown_yml()] to write to
#' `_pkgdown.yml`
#'
#'
#' @template describe_yml_param
#' @template describe_dots_param
#' @param as_is Logical. Use the `output_format` and options that you have
#'   specified?
#' @param extension The output extension, e.g. "pdf".
#'
#' @template describe_yml_output
#' @export
#'
#' @examples
#'
#' yml_empty() %>%
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


#' @param site_title The title of the website (by default, this is the package
#'   name). Note that the actual YAML is `title` (specified as `site_title` to
#'   avoid duplication with content titles).
#' @param destination The path where the site should be rendered ("docs/" by
#'   default)
#' @param url URL where the site will be published; setting the URL will allow
#'   other pkgdown sites to link to your site when needed, generate a
#'   `sitemap.xml` to increase the searchability of your site, and generate a
#'   `CNAME`.
#' @param toc_depth The depth of the headers included in the Table of Contents.
#'   Note that the actual YAML is `depth` and is nested under `toc`.
#'
#' @export
#' @rdname yml_pkgdown
yml_pkgdown_opts <- function(
  .yml,
  site_title = yml_blank(),
  destination = yml_blank(),
  url = yml_blank(),
  toc_depth = yml_blank()
) {
  pkgdown_opts <- list(
    title = site_title,
    destination = destination,
    url = url,
    toc = list(depth = toc_depth) %>% purrr::discard(is_yml_blank)
  ) %>%
    purrr::discard(is_yml_blank)

  warn_if_duplicate_fields(.yml, pkgdown_opts)
  .yml[names(pkgdown_opts)] <- pkgdown_opts

  .yml
}


#' @param mode The development mode of the site, one of: "auto", "release",
#'   "development", or "unreleased". `development` controls where the site is
#'   built; the color of the package version; the optional tooltip associated
#'   with the version; and the indexing of the site by search engines. See
#'   `?pkgdown::build_site()` for more details.
#' @param dev_destination The subdirectory used for the development site, which
#'   defaults to "dev/". Note that the actual YAML is `destination` and is
#'   nested under `development`.
#' @param version_label Label to display for "development" and "unreleased"
#'   mode. One of: "danger" (the default), "default", "info", or "warning".
#' @param version_tooltip A custom message to include in the version tooltip
#'
#' @export
#' @rdname yml_pkgdown
yml_pkgdown_development <- function(
  .yml,
  mode = yml_blank(),
  dev_destination = yml_blank(),
  version_label = yml_blank(),
  version_tooltip = yml_blank()
) {
  pkgdown_development_opts <- list(
    mode = mode,
    destination = dev_destination,
    version_label = version_label,
    version_tooltip = version_tooltip
  ) %>%
    purrr::discard(is_yml_blank)

  warn_if_duplicate_fields(.yml, pkgdown_development_opts)
  .yml[names(pkgdown_development_opts)] <- pkgdown_development_opts

  .yml
}

#' @param bootswatch A bootswatch theme for the site. See the options at
#'   <https://rstudio.github.io/shinythemes/>.
#'
#' @param ganalytics A Google Analytics tracking id
#' @param noindex Logical. Suppress indexing of your pages by web robots?
#' @param package an R package with with directories `inst/pkgdown/assets` and
#'   `inst/pkgdown/templates` to override the default templates and add
#'   additional assets; alternatively, you can specify this in `path` and
#'   `assets`
#' @param path A path to templates with which to override the default pkgdown
#'   templates
#' @param assets A path to additional assets to include
#' @param default_assets Logical. Include default assets?
#'
#' @export
#' @rdname yml_pkgdown
yml_pkgdown_template <- function(
  .yml,
  bootswatch = yml_blank(),
  ganalytics = yml_blank(),
  noindex = yml_blank(),
  package = yml_blank(),
  path = yml_blank(),
  assets = yml_blank(),
  default_assets = yml_blank()
) {
  pkgdown_template_opts <- list(
    bootswatch = bootswatch,
    ganalytics = ganalytics,
    noindex = noindex,
    package = package,
    path = path,
    assets = assets,
    default_assets = default_assets
  ) %>%
    purrr::discard(is_yml_blank)

  warn_if_duplicate_fields(.yml, pkgdown_template_opts)
  .yml[names(pkgdown_template_opts)] <- pkgdown_template_opts

  .yml
}


#' @param title The title of the article, reference, tutorial, or other resource
#' @param desc A description of the article or reference
#' @param contents The contents, which can also be dplyr-style tidy selectors
#'   (e.g `"contains('index')"`).
#' @param exclude What to exclude of the what's captured by `contents`
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


#' @param one_page Logical. Create one page per release for `NEWS.md`?
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


#' @param name The name of the file
#' @param tutorial_url The tutorial URL to embed in an iframe
#' @param source A URL to the source code of the tutorial
#'
#' @export
#' @rdname yml_pkgdown
yml_pkgdown_tutorial <- function(.yml, ...) {
  warn_if_duplicate_fields(.yml, list(references = ""))
  .yml$references <- c(...)
  .yml
}

#' @export
#' @rdname yml_pkgdown
pkgdown_tutorial <- function(
  name = yml_blank(),
  title = yml_blank(),
  tutorial_url = yml_blank(),
  source = yml_blank(),
  ...
) {
  list(
    name = name,
    title = title,
    url = tutorial_url,
    source = source,
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
#' @param doc_url the URL specifying the location of your documentation. Note that the actual YAML field is `url` but is nested.
#' @export
#' @rdname yml_pkgdown
yml_pkgdown_docsearch <- function(.yml, api_key = yml_blank(), index_name = yml_blank(), doc_url = yml_blank()) {
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
    url = doc_url
  ) %>%
    purrr::discard(is_yml_blank)

  warn_if_duplicate_fields(.yml, docsearch)
  .yml[names(docsearch)] <- docsearch

  .yml
}

