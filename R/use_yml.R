#' Copy YAML code to your clipboard
#'
#' `use_yml()` takes a `yml` object and puts the resulting YAML on your
#' clipboard to paste into an R Markdown or YAML file. By default, `use_yml()`
#' uses the most recently printed YAML via `last_yml()`.
#'
#' @param .yml A `yml` object, the most recently printed by default.
#'
#' @return `use_yml()` invisibly returns the input `yml` object
#' @export
#'
#' @examples
use_yml <- function(.yml = last_yml()) {
  return_yml_code(.yml)
}

#' @rdname use_yml
#' @export
use_rmarkdown <- function(.yml = last_yml(), path, template = NULL) {
  if (!is.null(template)) {
    existing_header <- read_yaml(template)
    printed_yaml <- existing_header %>%
      as_yml() %>%
      combine_yml(.yml) %>%
      capture_yml()

    rmarkdown_header <- c(printed_yaml, "\n")

    usethis::write_over(path, rmarkdown_header)
    rstudioapi::navigateToFile(path)

    return(invisible(.yml))
  }

  printed_yaml <- capture_yml(.yml)
  rmarkdown_template <- c(printed_yaml, "\n")

  usethis::write_over(path, rmarkdown_template)
  rstudioapi::navigateToFile(path)

  invisible(.yml)
}

combine_yml <- function(x, y) {
  warn_if_duplicate_fields(x, y)
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
#' @param x
#' @param path
#'
#' @return
#' @export
#'
#' @examples
#' @rdname use_file_yml
use_yml_file <- function(x = NULL, path) {
  write_yml_file(x, path)
}

#' @export
#' @rdname use_file_yml
use_output_yml <- function(x = NULL, path = ".") {
  file_path <- "_output.yml"
  if (path != ".") file_path <- file.path(p, file_path)

  write_yml_file(x, file_path)
}

#' @export
#' @rdname use_file_yml
use_site_yml <- function(x = NULL, path = ".") {
  file_path <- "_site.yml"
  if (path != ".") file_path <- file.path(p, file_path)

  write_yml_file(x, file_path)
}

#' @export
#' @rdname use_file_yml
use_pkgdown_yml <- function(x = NULL, path = ".") {
  file_path <- "_pkgdown.yml"
  if (path != ".") file_path <- file.path(p, file_path)

  write_yml_file(x, file_path)
}

#' @export
#' @rdname use_file_yml
use_bookdown_yml <- function(x = NULL, path = ".") {
  file_path <- "_bookdown.yml"
  if (path != ".") file_path <- file.path(p, file_path)

  write_yml_file(x, file_path)
}


write_yml_file <- function(x, path) {
  if (file.exists(path)) {
    question <- glue::glue("Overwrite pre-existing file {usethis::ui_path(path)}?")
    go_ahead <- usethis::ui_yeah(question)

    if (!go_ahead) return(invisible(path))
    fs::file_delete(path)
  }

  if (!is.null(x)) {
    yml_txt <- yaml::as.yaml(
      x,
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


#' Title
#'
#' @param .yml
#'
#' @return
#' @export
#'
#' @examples
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

#' Title
#'
#' @return
#' @export
#'
#' @examples
get_yml_defaults <- function() {
  .yml <- getOption("ymlthis.default_yml")
  if (is.null(.yml)) return(NULL)

  if (is.character(.yml)) .yml <- yaml::yaml.load(.yml)

  as_yml(.yml)
}

raw_yml <- function(x) {
  if (isTRUE(getOption("knitr.in.progress"))) return(knitr::asis_output(x))
  x
}
