#' Set Top-level YAML options for blogdown
#'
#' YAML in blogdown comes from a variety of sources. Most YAML will be for your
#' posts, as described in the [blogdown
#' book](https://bookdown.org/yihui/blogdown/content.html#yaml-metadata)).
#' Common R Markdown fields can be used, but there are two other main sources
#' for YAML fields: Hugo itself and the Hugo theme you are using. Hugo has
#' numerous top-level YAML to control the output (see the [Hugo
#' documentation](https://gohugo.io/content-management/front-matter/)).
#' `yml_blogdown_opts()` supports Hugo YAML. Your Hugo theme may also add fields
#' to use. To find YAML specific to your theme, see [blogdown_template()]. In
#' addition to these sources of YAML, the configuration file for your blog can
#' also be in YAML, but this is not very common; most use a `config.toml` file,
#' based on TOML (see the [blogdown
#' book](https://bookdown.org/yihui/blogdown/configuration.html) for more).
#'
#' @template describe_yml_param
#' @param draft Logical. Set post as a draft? Draft posts will not be rendered
#'   if the site is built via `blogdown::build_site()` or
#'   `blogdown::hugo_build()` but will be rendered in the local preview mode.
#'   See [Section D.3 of the blogdown
#'   book](https://bookdown.org/yihui/blogdown/local-preview.html#local-preview).
#'
#' @param publishdate A future date to publish the post. Future posts are only
#'   rendered in the local preview mode
#' @param weight This field can take a numeric value to tell Hugo the order of
#'   pages when sorting them, e.g., when you generate a list of all pages under
#'   a directory, and two posts have the same date, you may assign different
#'   weights to them to get your desired order on the list
#' @param slug A character string used as the tail of the post URL. It is
#'   particularly useful when you define custom rules for permanent URLs. See
#'   [Section 2.2.2 of the blogdown
#'   book](https://bookdown.org/yihui/blogdown/configuration.html#options).
#' @param aliases A character vector of one or more aliases (e.g., old published
#'   paths of renamed content) that will be created in the output directory
#'   structure
#' @param audio A character vector of paths to audio files related to the page
#' @param date The date assigned to this page. This is usually fetched from the
#'   `date` field in front matter, but this behavior is configurable.
#' @param description The description for the content
#' @param expiration_date the date at which the content should no longer be
#'   published by Hugo. Note that the actual YAML field is `expiryDate`
#' @param headless if `TRUE`, sets a leaf bundle to be
#'   [headless](https://gohugo.io/content-management/page-bundles/#headless-bundle).
#'
#' @param images A character vector of paths to images related to the page
#' @param keywords A character vector of the keywords for the content.
#' @param layout The layout Hugo should use while rendering the content. By
#'   default, `layout` matches `type` and is thus based on the directory.
#'   However, it's possible to use additional layouts within a type. See [Hugo's
#'   Defining a Content Type
#'   documentation](https://gohugo.io/content-management/types/#defining-a-content-type).
#'
#' @param lastmod The date the content was last modified at
#' @param link_title used for creating links to content.  Note that the actual
#'   YAML field is `linkTitle`
#' @param resources A named list. Used for configuring page bundle resources.
#'   See [Hugo's Page Resources
#'   documentation](https://gohugo.io/content-management/page-resources/)
#' @param series A character vector of series this page belongs to
#' @param summary A summary of the content in the `.Summary` Hugo page variable;
#'   see the
#'   [content-summaries](https://gohugo.io/content-management/summaries/)
#'   section of Hugo's documentation.
#' @param title The title for the content
#' @param type The type of the content, which is based on the from the directory
#'   of the content if not specified
#' @param url The full path to the content from the web root
#' @param videos A character vector of paths to videos related to the page
#' @template describe_dots_param
#'
#' @template describe_yml_output
#' @export
#'
#' @examples
#'
#' yml() %>%
#'   yml_blogdown_opts(
#'     draft = TRUE,
#'     slug = "blog-post"
#'   )
yml_blogdown_opts <- function(
  .yml,
  draft = yml_blank(),
  publishdate = yml_blank(),
  weight = yml_blank(),
  slug = yml_blank(),
  aliases = yml_blank(),
  audio = yml_blank(),
  date = yml_blank(),
  description = yml_blank(),
  expiration_date = yml_blank(),
  headless = yml_blank(),
  images = yml_blank(),
  keywords = yml_blank(),
  layout = yml_blank(),
  lastmod = yml_blank(),
  link_title = yml_blank(),
  resources = yml_blank(),
  series = yml_blank(),
  summary = yml_blank(),
  title = yml_blank(),
  type = yml_blank(),
  url = yml_blank(),
  videos = yml_blank(),
  ...
) {
  blogdown_opts <- list(
    draft = draft,
    publishdate = publishdate,
    weight = weight,
    slug = slug,
    aliases = aliases,
    audio = audio,
    date = date,
    description = description,
    draft = draft,
    expiryDate = expiration_date,
    headless = headless,
    images = images,
    keywords = keywords,
    layout = layout,
    lastmod = lastmod,
    linkTitle = link_title,
    resources = resources,
    series = series,
    summary = summary,
    title = title,
    type = type,
    url = url,
    videos = videos,
    ...
  ) %>%
    purrr::discard(is_yml_blank)

  warn_if_duplicate_fields(.yml, blogdown_opts)
  .yml[names(blogdown_opts)] <- blogdown_opts

  .yml
}

#' Create YAML based on blogdown theme archetypes
#'
#' `blogdown_template()` creates YAML based on your blogdown theme archetypes.
#' blogdown is based on Hugo, which supports many custom themes. Each theme uses
#' YAML in a different way. However, many come with archetypes that define the
#' YAML or TOML. To find out which types your theme has, use
#' `blogdown_archetypes()` to see what's available. Use `blogdown_template()` to
#' specify the archetype and it will convert the template to YAML that you can
#' use in your post.
#'
#' @param type an archetype
#' @param path the path to your blogdown site
#' @param theme the theme to check for archetypes. By default,
#'   `blogdown_template()` will attempt to read your theme from your `config`
#'   file.
#'
#' @template describe_yml_output
#' @export
blogdown_template <- function(type, path = ".", theme = NULL) {
  stop_if_blogdown_not_installed()
  on.exit(unlink_temporary_dir())

  if (is.null(theme)) theme <- get_theme(path)

  archetype_path <- file.path(path, "themes", theme, "archetypes")

  if (!file.exists(file.path(archetype_path, paste0(type, ".md")))) {
    stop("archetype ", type, " does not exist", call. = FALSE)
  }

  fs::dir_create(file.path(temporary_dir(), "content"))
  fs::file_copy(
    file.path(archetype_path, paste0(type, ".md")),
    file.path(temporary_dir(), "content")
  )

  writeLines(
    c("baseurl = \"/\"", "builddrafts = true"),
    file.path(temporary_dir(), "config.toml")
  )

  file_to_yamlify <- file.path(
    temporary_dir(),
    "content",
    paste0(type, ".md")
  )
  clean_archetype_files(file_to_yamlify)

  withr::with_dir(
    temporary_dir(),
    blogdown::hugo_cmd(
      args = c("convert", "toYAML", "--unsafe"),
      stdout = TRUE
    )
  )

  readLines(file.path(temporary_dir(), "content"))

  post_yml <- yaml::yaml.load_file(file_to_yamlify) %>%
    as_yml()

  post_yml
}

#' @export
#' @rdname blogdown_template
blogdown_archetypes <- function(path = ".", theme = NULL) {
  if (is.null(theme)) theme <- get_theme(path)
  file.path(path, "themes", theme, "archetypes") %>%
    fs::dir_ls() %>%
    basename() %>%
    stringr::str_remove(".md$")
}

clean_archetype_files <- function(path) {
  readLines(path) %>%
    #  remove {{expr}} functions
    stringr::str_replace_all('\\{\\{.+\\}\\}', '\\"\\"') %>%
    stringr::str_replace_all('\\"\\"\\"\\"', '\\"\\"') %>%
    writeLines(path)
}

get_theme <- function(path) {
  config_path <- fs::dir_ls(path) %>%
    stringr::str_subset("config\\.(toml|yaml|yml)$")
  config_file <- readLines(config_path)
  theme <- config_file %>%
    stringr::str_subset("^theme") %>%
    stringr::str_remove_all("theme|=|\\s*") %>%
    stringr::str_remove_all("\"|\'")

  theme
}
