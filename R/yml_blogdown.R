#' Set Top-level YAML options for blogdown
#'
#' YAML in blogdown comes from a variety of sources. Technically, the
#' configuration file for your blog can be in YAML, but this is not very
#' common; most use `config.toml`, based on TOML (see the [blogdown
#' book](https://bookdown.org/yihui/blogdown/configuration.html) for more). Most
#' YAML will be for your posts (which the [blogdown
#' book](https://bookdown.org/yihui/blogdown/content.html#yaml-metadata) also
#' describes). Common R Markdown fields can be used, but there are two other
#' main sources for YAML fields: Hugo itself and the theme you are using. Hugo
#' has numerous top-level YAML to control the output (see the [Hugo
#' documentation](https://gohugo.io/content-management/front-matter/)), and your
#' theme may add many more or none. `yml_blogdown_opts()` supports Hugo YAML. To
#' use YAML specific to your theme, see [blogdown_template()]
#'
#' @template describe_yml_param
#' @param draft You can mark a document as a draft by setting `TRUE`. Draft
#'   posts will not be rendered if the site is built via
#'   `blogdown::build_site()` or `blogdown::hugo_build()`, but will be rendered
#'   in the local preview mode. See [Section D.3 of the Blogdown
#'   book](https://bookdown.org/yihui/blogdown/local-preview.html#local-preview).
#' @param publishdate You may specify a future date to publish a post. Similar
#'   to draft posts, future posts are only rendered in the local preview mode
#' @param weight This field can take a numeric value to tell Hugo the order of
#'   pages when sorting them, e.g., when you generate a list of all pages under
#'   a directory, and two posts have the same date, you may assign different
#'   weights to them to get your desired order on the list
#' @param slug A character string as the tail of the URL. It is particularly
#'   useful when you define custom rules for permanent URLs See [Section 2.2.2
#'   of the Blogdown
#'   book](https://bookdown.org/yihui/blogdown/configuration.html#options).
#' @param aliases an array of one or more aliases (e.g., old published paths of
#'   renamed content) that will be created in the output directory structure
#' @param audio an array of paths to audio files related to the page
#' @param date the datetime assigned to this page. This is usually fetched from
#'   the `date` field in front matter, but this behaviour is configurable.
#' @param description the description for the content
#' @param expiration_date the datetime at which the content should no longer be
#'   published by Hugo; expired content will not be rendered unless the
#'   --buildExpired flag is passed to the hugo command.
#' @param headless if true, sets a leaf bundle to be
#'   [headless](https://gohugo.io/content-management/page-bundles/#headless-bundle).
#' @param images an array of paths to images related to the page
#' @param keywords the meta keywords for the content.
#' @param layout the layout Hugo should select from the lookup order when
#'   rendering the content. If a type is not specified in the front matter, Hugo
#'   will look for the layout of the same name in the layout directory that
#'   corresponds with a content’s section. See [Defining a Content
#'   Type](https://gohugo.io/content-management/types/#defining-a-content-type).
#' @param lastmod the datetime at which the content was last modified.
#' @param link_title used for creating links to content
#' @param resources used for configuring page bundle resources. See [Page
#'   Resources](https://gohugo.io/content-management/page-resources/)
#' @param series an array of series this page belongs to
#' @param summary text used when providing a summary of the article in the
#'   .Summary page variable; details available in the
#'   [content-summaries](https://gohugo.io/content-management/summaries/)
#'   section.
#' @param title the title for the content
#' @param type the type of the content; this value will be automatically derived
#'   from the directory (i.e., the
#'   [section](https://gohugo.io/content-management/sections/)) if not specified
#'   in front matter
#' @param url the full path to the content from the web root. It makes no
#'   assumptions about the path of the content file. It also ignores any
#'   language prefixes of the multilingual feature.
#' @param videos an array of paths to videos related to the page
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
#' specify the archetype and it will conver the template to YAML that you can
#' use in your post.
#'
#' @param type an archetype
#' @param path a path that contains your blogdown site
#' @param theme the theme to check for archetypes. By default,
#'   `blogdown_template()` will attempt to read your theme from `config.toml`.
#'
#' @template describe_yml_output
#' @export
blogdown_template <- function(type, path = ".", theme = NULL) {
  stop_if_blogdown_not_installed()
  on.exit(unlink(tempdir(), recursive = TRUE, force = TRUE))

  if (is.null(theme)) theme <- get_theme(path)

  archetype_path <- file.path(path, "themes", theme, "archetypes")

  if (!file.exists(file.path(archetype_path, paste0(type, ".md")))) {
    stop("archetype ", type, " does not exist", call. = FALSE)
  }

  fs::dir_create(file.path(tempdir(), "content"))
  fs::file_copy(
    file.path(archetype_path, paste0(type, ".md")),
    file.path(tempdir(), "content")
  )

  readr::write_lines(
    c("baseurl = \"/\"", "builddrafts = true"),
    file.path(tempdir(), "config.toml")
  )

  file_to_yamlify <- file.path(
    tempdir(),
    "content",
    paste0(type, ".md")
  )
  clean_archetype_files(file_to_yamlify)

  withr::with_dir(
    tempdir(),
    blogdown::hugo_cmd(
      args = c("convert", "toYAML", "--unsafe"),
      stdout = TRUE
    )
  )

  readLines(file.path(tempdir(), "content"))

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
  readr::read_file(path) %>%
    #  has trouble with these {{}} templates
    stringr::str_replace_all('\\{\\{.+\\}\\}', '\\"\\"') %>%
    stringr::str_replace_all('\\"\\"\\"\\"', '\\"\\"') %>%
    readr::write_lines(path)
}

get_theme <- function(path) {
  config_toml <- readLines(file.path(path, "config.toml"))
  theme <- config_toml %>%
    stringr::str_subset("^theme") %>%
    stringr::str_remove_all("theme|=|\\s*") %>%
    stringr::str_remove_all("\"|\'")

  theme
}
