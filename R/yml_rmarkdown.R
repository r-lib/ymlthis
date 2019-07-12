#' Set R Markdown to use Shiny
#'
#' The `runtime` field lets you use Shiny in your R Markdown document, making it
#' interactive. See the [R Markdown
#' book](https://bookdown.org/yihui/rmarkdown/interactive-documents.html) for
#' examples.
#'
#' @template describe_yml_param
#' @param runtime The runtime target for rendering. `static`, the default,
#'   renders static documents; `shiny` allows you to include use Shiny in your
#'   document. `shiny_prerendered` is a subset of the `shiny` runtime that
#'   allows pre-rendering of app components (see the [R Markdown
#'   site](https://rmarkdown.rstudio.com/authoring_shiny_prerendered.html) for
#'   more)
#'
#' @template describe_yml_output
#' @export
#'
#' @examples
#'
#' yml() %>%
#'   yml_runtime("shiny")
#'
#' @family R Markdown
#' @family shiny
yml_runtime <- function(.yml, runtime = c("static", "shiny", "shiny_prerendered")) {
   warn_if_duplicate_fields(.yml, list(runtime = ""))
  .yml$runtime <- match.arg(runtime)

  .yml
}

#' Remove intermediate rendering files
#'
#' R Markdown may create many documents while rendering the final product, for
#' instance by using knitr to turn the R Markdown file to a Markdown file and
#' then using Pandoc to convert to the final output. The `clean` field tells R
#' Markdown whether or not to remove these files.
#'
#' @template describe_yml_param
#' @param clean Logical. Remove intermediate files that are created while making
#'   the R Markdown document?
#'
#' @template describe_yml_output
#' @export
#'
#' @examples
#'
#' yml() %>%
#'   # keep intermediate files
#'   yml_clean(FALSE)
#'
#' @family R Markdown
yml_clean <- function(.yml, clean) {
  stop_if_not_type(clean, "logical")
  warn_if_duplicate_fields(.yml, list(clean = ""))
  .yml$clean <- clean

  .yml
}

#' Set up a package vignette
#'
#' To use an R Markdown file as a vignette, you need to specify an output format
#' appropriate for inclusion in a package (for example, the lightweight
#' `html_vignette()`) and to specify the `vignette` field, which specifies the
#' title, engine, and encoding type of the vignette.
#'
#' @template describe_yml_param
#' @param title The title of the vignette
#' @param engine The rendering engine for the vignette ("knitr::rmarkdown" by
#'   default)
#' @param encoding The character encoding for the document ("UTF-8" by default).
#'
#' @template describe_yml_output
#' @export
#'
#' @examples
#'
#' yml() %>%
#'   yml_output(html_vignette()) %>%
#'   yml_vignette("An introduction to R Markdown")
#'
#' @family R Markdown
yml_vignette <- function(.yml, title, engine = "knitr::rmarkdown", encoding = "UTF-8") {
  warn_if_duplicate_fields(.yml, list(vignette = ""))
  .yml$vignette <- glue::glue(
    "%\\VignetteIndexEntry{<<title>>} \n\\
    %\\VignetteEngine{<<engine>>} \n\\
    %\\VignetteEncoding{<<encoding>>})",
    .open = "<<",
    .close = ">>"
  )

  .yml
}

#' Add site options for _site.yml and navbars for R Markdown websites
#'
#' R Markdown has a simple website builder baked in (see the R [Markdown
#' book](https://bookdown.org/yihui/rmarkdown/rmarkdown-site.html#site_navigation)
#' for a detailed description). An R Markdown website must have at least have an
#' index.Rmd file and a _site.yml file (which can be empty). YAML included in
#' _site.yml will apply to all R Markdown files related to the website, so
#' setting the output format here will apply across the website. R Markdown
#' websites also support navbars, which you can specify with YAML (see
#' [yml_navbar()], as well as ?rmarkdown::render_site and
#' ?rmarkdown::html_document). Pass `navbar_page()` to the `left` or `right` field to set up page tabs and use `navbar_separator()` to include a separators. In addition to writing YAML with `yml_*()`
#' functions, `use_site_yml()` will take the resulting `yml` object and write it
#' to a `_site.yml` file for you.
#'
#' @template describe_yml_param
#' @param name The name of the website
#' @param favicon Path to a file to use as the favicon
#' @param output_dir directory to copy site content into ("_site" is the default
#'   if none is specified)
#' @param include,exclude Files to include or exclude from the copied into
#'   `output_dir`. You can use `*` to indicate a wildcard selection, e.g.
#'   "*.csv".
#' @param new_session Logical. Should each website file to be rendered in a new
#'   R session?
#' @template describe_dots_param
#'
#' @template describe_yml_output
#' @export
#'
#' @examples
#' yml(author = FALSE, date = FALSE) %>%
#'   yml_site_opts(
#'     name = "my-website",
#'     output_dir =  "_site",
#'     include = "demo.R",
#'     exclude = c("docs.txt", "*.csv")
#'   ) %>%
#'   yml_navbar(
#'     title = "My Website",
#'     left = list(
#'       navbar_page("Home", href = "index.html"),
#'       navbar_page(navbar_separator(), href = "about.html")
#'     )
#'   ) %>%
#'   yml_output(html_document(toc = TRUE, highlight = "textmate"))
#'
#' # optionally pass to `use_site_yml()` to write this YAML to `_site.yml`
#'
#' @family R Markdown
#' @family websites
#' @seealso [use_site_yml()] [use_navbar_yml()]
yml_site_opts <- function(
  .yml,
  name = yml_blank(),
  favicon = yml_blank(),
  output_dir = yml_blank(),
  include = yml_blank(),
  exclude = yml_blank(),
  new_session = yml_blank(),
  ...
) {
  site_opts <- list(
    name = name,
    favicon = favicon,
    output_dir = output_dir,
    include = include,
    exclude = exclude,
    new_session = new_session,
    ...
  ) %>%
    purrr::discard(is_yml_blank)

  warn_if_duplicate_fields(.yml, site_opts)
  .yml[names(site_opts)] <- site_opts

  .yml
}

#' @param title The title of the website
#' @param type The color scheme for the navigation bar: either "default" or "inverse".
#' @param left,right the side of the navbar a `navbar_page()` should go (see example)
#' @export
#' @rdname yml_site_opts
yml_navbar <- function(.yml, title = yml_blank(), type = yml_blank(),
                       left = yml_blank(), right = yml_blank(), ...) {
  navbar <- list(
    title = title,
    type = type,
    left = left,
    right = right,
    ...
  ) %>%
    purrr::discard(is_yml_blank)

  warn_if_duplicate_fields(.yml, list(navbar = ""))
  .yml$navbar <- navbar

  .yml
}

#' @param text The link text
#' @param href The link URL
#' @param icon An icon to include
#' @param menu drop-down menus specified by including another `navbar_page()`
#'
#' @export
#' @rdname yml_site_opts
navbar_page <- function(text = yml_blank(), href = yml_blank(), icon = yml_blank(), menu = yml_blank(), ...) {
  list(
    text = text,
    href = href,
    icon = icon,
    menu = menu,
    ...
  ) %>%
    purrr::discard(is_yml_blank)
}

#' @export
#' @rdname yml_site_opts
navbar_separator <- function() {
  "---------"
}
