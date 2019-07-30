#' Copy YAML code to your clipboard or write to a new R Markdown file
#'
#' `use_yml()` takes a `yml` object and puts the resulting YAML on your
#' clipboard to paste into an R Markdown or YAML file. `use_rmarkdown()` takes
#' the `yml` object and writes it to a new R Markdown file. You can add text to
#' include in the body of the file. If it's not specified, `use_rmarkdown()`
#' will use [`setup_chunk()`] by default. You can also set a default for `body`
#' using `options(ymlthis.rmd_body = "{your text}")`; see [use_rmd_defaults()].
#' Together with specifying default YAML (see [use_yml_defaults()]),
#' `use_rmarkdown()` also serves as an ad-hoc way to make R Markdown templates.
#' You may also supply `use_rmarkdown()` with an existing R Markdown file from
#' which to read the YAML header; the YAML header from the template is then
#' combined with `.yml`, if it's supplied, and written to a new file.
#' `use_index_rmd()` is a wrapper around `use_rmarkdown()` that specifically
#' writes to a file called `index.Rmd`. By default, `use_yml()` and
#' `use_rmarkdown()` use the most recently printed YAML via [`last_yml()`].
#'
#' @template describe_yml_param
#' @param path A file path to write R Markdown file to
#' @param template An existing R Markdown file to read YAML from
#' @param include_yaml Logical. Include the template YAML?
#' @param include_body Logical. Include the template body?
#' @param body A character vector to use in the body of the R Markdown file. If
#'   no template is set, checks `getOption("ymlthis.rmd_body")` (see
#'   [`use_rmd_defaults()`]) and otherwise uses [`setup_chunk()`].
#'
#' @return `use_yml()` invisibly returns the input `yml` object
#' @export
#' @seealso [`code_chunk()`] [`setup_chunk()`]
use_yml <- function(.yml = last_yml()) {
  return_yml_code(.yml)
}


#' @rdname use_yml
#' @export
use_rmarkdown <- function(.yml = last_yml(), path, template = NULL, include_yaml = TRUE, include_body = TRUE, body = NULL) {

  if (!is.null(template) && fs::is_dir(template)) {
    template_skeleton <- file.path(template, "skeleton", "skeleton.Rmd")
    if (!file.exists(template_skeleton)) {
      stop("A directory must include an R Markdown template in `skeleton/skleton.Rmd`", call. = FALSE)
    }
    template <- template_skeleton
  }

  if (!is.null(template) && include_yaml) {
    existing_yaml <- read_rmd(template) %>%
      yml_load()
    .yml <- combine_yml(.yml, existing_yaml)
  }
  rmarkdown_txt <- capture_yml(.yml)

  if (is.null(template) && is.null(body)) body <- getOption("ymlthis.rmd_body")
  if (is.null(template) && is.null(body)) body <- c("", setup_chunk())

  rmarkdown_txt <- c(rmarkdown_txt, body)
  if (!is.null(template) && include_body) {
    existing_body <- read_rmd(template, output = "body")
    rmarkdown_txt <- c(rmarkdown_txt, existing_body)
  }

  usethis::write_over(path, rmarkdown_txt)
  if (rstudioapi::isAvailable()) rstudioapi::navigateToFile(path, line = 2)

  invisible(path)
}

#' @rdname use_yml
#' @export
use_index_rmd <- function(.yml = last_yml(), path = ".", template = NULL) {
  index_rmd_path <- file_path(path, "index.Rmd")
  use_rmarkdown(.yml = .yml, path = index_rmd_path, template = template)
}

combine_yml <- function(x, y) {
  x[names(y)] <- y

  x
}
return_yml_code <- function(.yml) {
  yaml_text <- capture_yml(.yml)
  usethis::ui_code_block(yaml_text)
  usethis::ui_todo("Paste into R Markdown or YAML file")

  invisible(.yml)
}

#' Write YAML to file
#'
#' Write `yml` objects to a file. `use_yml_file()` writes to any given file
#' name. `use_output_yml()` creates file `_output.yml`, which can be used by
#' multiple R Markdown documents. All documents located in the same directory as
#' `_output.yml` will inherit its options. Options defined within document YAML
#' headers will override those specified in `_output.yml`. `use_site_yml`
#' creates `_site.yml` for use with R Markdown websites and third-party tools
#' like the distill package (see [the R Markdown book for
#' more](https://bookdown.org/yihui/rmarkdown/rmarkdown-site.html#)).
#' `use_navbar_yml` is a special type of site YAML that only specifies the
#' navbar in `_navbar.yml` `use_pkgdown_yml()` and `use_bookdown_yml()` write
#' YAML files specific to those packages; see the
#' [pkgdown](https://pkgdown.r-lib.org/articles/pkgdown.html) and
#' [blogdown](https://bookdown.org/yihui/bookdown/configuration.html)
#' documentation for more.
#'
#' @template describe_yml_param
#' @param path a file path to write the file to
#' @param build_ignore Logical. Should the file be added to the `.Rbuildignore`
#'   file?
#' @param git_ignore Logical. Should the file be added to the `.gitignore` file?
#'
#' @seealso yml_bookdown_opts yml_bookdown_site yml_pkgdown yml_pkgdown_articles
#'   yml_pkgdown_docsearch yml_pkgdown_figures yml_pkgdown_news
#'   yml_pkgdown_reference
#' @export
#' @rdname use_file_yml
use_yml_file <- function(.yml = NULL, path, build_ignore = FALSE, git_ignore = FALSE) {
  write_yml_file(.yml, path, build_ignore = build_ignore, git_ignore = git_ignore)
}

#' @export
#' @rdname use_file_yml
use_output_yml <- function(.yml = NULL, path = ".", build_ignore = FALSE, git_ignore = FALSE) {
  yml_file_path <- file_path(path, "_output.yml")

  write_yml_file(.yml, yml_file_path, build_ignore = build_ignore, git_ignore = git_ignore)
}

#' @export
#' @rdname use_file_yml
use_site_yml <- function(.yml = NULL, path = ".", build_ignore = FALSE, git_ignore = FALSE) {
  yml_file_path <- file_path(path, "_site.yml")

  write_yml_file(.yml, yml_file_path, build_ignore = build_ignore, git_ignore = git_ignore)
}

#' @export
#' @rdname use_file_yml
use_navbar_yml <- function(.yml = NULL, path = ".", build_ignore = FALSE, git_ignore = FALSE) {
  yml_file_path <- file_path(path, "_navbar.yml")

  write_yml_file(.yml, yml_file_path, build_ignore = build_ignore, git_ignore = git_ignore)
}

#' @export
#' @rdname use_file_yml
use_pkgdown_yml <- function(.yml = NULL, path = ".", build_ignore = TRUE, git_ignore = FALSE) {
  yml_file_path <- file_path(path, "_pkgdown.yml")

  write_yml_file(.yml, yml_file_path, build_ignore = build_ignore, git_ignore = git_ignore)
}

#' @export
#' @rdname use_file_yml
use_bookdown_yml <- function(.yml = NULL, path = ".", build_ignore = FALSE, git_ignore = FALSE) {
  yml_file_path <- file_path(path, "_bookdown.yml")

  write_yml_file(.yml, yml_file_path, build_ignore = build_ignore, git_ignore = git_ignore)
}


write_yml_file <- function(.yml, path, build_ignore = FALSE, git_ignore = FALSE) {
  if (build_ignore) usethis::use_build_ignore(path)
  if (git_ignore) usethis::use_git_ignore(path)

  if (file.exists(path)) {
    question <- glue::glue("Overwrite pre-existing file {usethis::ui_path(path)}?")
    go_ahead <- usethis::ui_yeah(question)

    if (!go_ahead) return(invisible(path))
    fs::file_delete(path)
  }

  if (!is.null(.yml)) {
    yml_txt <- yaml::as.yaml(
      .yml,
      handlers = yml_handlers(),
      column.major = FALSE
    )

    usethis::write_over(path, yml_txt)
    return(invisible(path))
  }

  fs::file_create(path)
  usethis::ui_done("Writing {usethis::ui_path(path)}")

  invisible(path)
}

file_path <- function(path, .file) {
  file.path(normalizePath(path), .file)
}


#' Set up default YAML
#'
#' `use_yml_defaults()` takes a `yml` object and places code on the clipboard
#' that will save the resulting YAML as the default for `yml()`. The code that
#' is placed on the clipboard is raw YAML passed to `ymlthis.default_yml` via
#' `options()`. Saving this code to your `.Rprofile` (see
#' [`usethis::edit_r_profile()`])) will allow [`yml()`] or
#' [`get_yml_defaults()`] to return the saved YAML. `use_rmd_defaults()` does
#' the same for `ymlthis.rmd_body`, which is used in [use_rmarkdown()] as the
#' body text of the created R Markdown file.
#'
#' @template describe_yml_param
#' @param x a character vector to use as the body text in [use_rmarkdown()].
#'
#' @seealso [yml()] [get_yml_defaults()]
#' @export
use_yml_defaults <- function(.yml) {
  if (!is_yml(.yml) && !is.character(.yml)) {
    usethis::ui_stop(
      "`{usethis::ui_code(.yml)}` must be a `{usethis::ui_code(yml)}` \\
      object or a `{usethis::ui_code(character)}` vector containing \\
      valid YAML text"
    )
  }

  if (is.character(.yml)) .yml <- as_yml(.yml)

  .yml_text <- capture_yml(.yml) %>%
    purrr::discard(~ .x == "---") %>%
    glue::glue_collapse(sep = "\n")

  .yml_code <- glue::glue("options(ymlthis.default_yml = \"{.yml_text}\")")

  usethis::ui_code_block(.yml_code)
  usethis::ui_todo(
    "Run interactively or paste into .Rprofile \\
   (perhaps using {usethis::ui_code('usethis::edit_r_profile()')})"
  )

  invisible(.yml)
}

#' @export
#' @rdname use_yml_defaults
use_rmd_defaults <- function(x) {
  rmd_text <- glue::glue_collapse(x, sep = "\n")
  usethis::ui_code_block("options(ymlthis.rmd_body = \"{rmd_text}\")")
  usethis::ui_todo(
    "Run interactively or paste into .Rprofile \\
   (perhaps using {usethis::ui_code('usethis::edit_r_profile()')})"
  )

  invisible(x)
}

#' @export
#' @rdname use_yml_defaults
get_yml_defaults <- function() {
  .yml <- getOption("ymlthis.default_yml")
  if (is.null(.yml)) return(NULL)

  if (is.character(.yml)) .yml <- yaml::yaml.load(.yml)

  as_yml(.yml)
}

#' @export
#' @rdname use_yml_defaults
get_rmd_defaults <- function() {
  getOption("ymlthis.rmd_body")
}

raw_yml <- function(x) {
  if (isTRUE(getOption("knitr.in.progress"))) return(knitr::asis_output(x))
  x
}
