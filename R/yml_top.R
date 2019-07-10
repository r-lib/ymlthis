#' Top-level R Markdown YAML Fields
#'
#' These functions add top-level YAML fields related to R Markdown documents.
#' Each takes a `yml` object and adds fields related to the function, as well as
#' checking for duplicate fields and (where possible) checking for valid
#' entries. `yml_toplevel()` is a catch-all function that will take any named R
#' object and put in the top level of the YAML; it checks for duplicate fields
#' but is unable to validate the input beyond that it is valid YAML.
#'
#' @template describe_yml_param
#' @param name A character vector, name of the author(s)
#' @param affiliation The author's affiliation; must match length of `name`,
#'   e.g. if `name` has length of two, `affiliation` must as well; use `NA` for
#'   authors that don't have an affiliation to include.
#' @param date The date; by default this is "``r format(Sys.Date())``", which
#'   will populate the date automatically.
#' @param format When the default `date` is used, the format passed to
#'   `[format.Date()]`.
#' @param title A character vector, tThe title of the document
#' @param subtitle A character vector, the subtitle of the document. Not all R
#'   Markdown formats use subtitles, so it may depend on what you use in the
#'   output field (see [yml_output()]). It is available in `pdf_document()`,
#'   `html_document()`, and `word_document()` by default.
#' @param abstract A character vector, the abstract. Long characters are
#'   automatically wrapped using valid YAML syntax. This field is not available
#'   in all output formats; it is available in `pdf_document()` and
#'   `html_document()` by default.
#' @param keywords A character vector of keywords. This field is not available
#'   in all output formats; it is available in `pdf_document()`,
#'   `html_document()`, `word_document()`, `odt_document()`, and
#'   `powerpoint_presentation()` by default.
#' @param subject A character vector, the subject of the document. This field is
#'   not available in all output formats; it is available in `pdf_document()`,
#'   `html_document()`, `word_document()`,  `odt_document()`, and
#'   `powerpoint_presentation()` by default.
#' @param description A character vector, a description of the document. This
#'   field is not available in all output formats; it is available in
#'   `word_document()`, `odt_document()`, and `powerpoint_presentation()` by
#'   default.
#' @param category A character vector, the category of the document. This field
#'   is not available in all output formats; it is available in
#'   `word_document()` and `powerpoint_presentation()` by default.
#' @param lang The document language using IETF language tags such as "en" or
#'   "en-US". The [Language subtag lookup
#'   tool](https://r12a.github.io/app-subtags/) can help find the appropriate
#'   tag.
#' @param ... any number of named R objects, most commonly a list or character
#'   vector
#'
#' @return a `yml` object
#' @export
#'
#' @examples
#' yml(author = FALSE, date = FALSE) %>%
#'   yml_author("Yihui Xie") %>%
#'   yml_date("02-02-2002") %>%
#'   yml_title("R Markdown: An Introduction") %>%
#'   yml_subtitle("Introducing ymlthis") %>%
#'   yml_abstract("This paper will discuss a very important topic") %>%
#'   yml_keywords(c("r", "reproducible research")) %>%
#'   yml_subject("R Markdown") %>%
#'   yml_description("An R Markdown reader") %>%
#'   yml_category("r") %>%
#'   yml_lang("en-US")
#'
yml_author <- function(.yml, name = NULL, affiliation = NULL, ...) {
  if (!is.null(name) && is.null(affiliation)) {
    stop_if_not_all_type(name, "character")
    .yml$author <- name
    return(.yml)
  }

  if (!is.null(name) && !is.null(affiliation)) {
    stop_if_not_all_type(name, "character")
    stop_if_not_all_type(affiliation, "character")
    #  use unnamed inner list to create `-` group:
    #  - author
    #    affiliation
    .yml$author <- purrr::map2(name, affiliation, ~author_list(.x, .y, ...))
    return(.yml)
  }

  author_list <- list(author = get_author_name(), ...)
  warn_if_duplicate_fields(.yml, author_list)
  .yml[names(author_list)] <- author_list

  .yml
}

author_list <- function(.x, .y, ...) {
  list(name = .x, affiliation = .y, ...) %>%
    purrr::discard(~is.na(.x))
}

get_author_name <- function() {
  name <- getOption("usethis.full_name")
  if (!is.null(name)) {
    return(name)
  }

  name <- getOption("devtools.name")
  if (!is.null(name) && name != "Your name goes here") {
    return(name)
  }

  name <- whoami::fullname(fallback = NA)
  if (!is.na(name)) {
    return(name)
  }

  usethis::ui_stop(
    "
    `{usethis::ui_code('name')}` argument is missing.
    Set it globally with {usethis::ui_code('options(usethis.full_name = \"My name\")')}\\
    probably in your {usethis::ui_path('.Rprofile')}.
    "
  )
}


#' @export
#' @rdname yml_author
yml_date <- function(.yml, date = NULL, format = "") {
  if (!is.null(date)) {
    .yml$date <- date
    return(.yml)
  }

  warn_if_duplicate_fields(.yml, list(date = ""))
  .yml$date <- format_sys_date(format = format)

  .yml
}

format_sys_date <- function(format = "") {
  if (format == "") {
    return("`r format(Sys.Date())`")
  }

  glue::glue("`r format(Sys.Date(), format = {format})`")
}

#' @export
#' @rdname yml_author
yml_title <- function(.yml, title) {
  stop_if_not_type(title, "character")
  warn_if_duplicate_fields(.yml, list(title = ""))
  .yml$title <- title

  .yml
}

#' @export
#' @rdname yml_author
yml_subtitle <- function(.yml, subtitle) {
  stop_if_not_type(subtitle, "character")
  warn_if_duplicate_fields(.yml, list(subtitle = ""))
  .yml$subtitle <- subtitle

  .yml
}

#' @export
#' @rdname yml_author
yml_abstract <- function(.yml, abstract) {
  stop_if_not_type(abstract, "character")
  warn_if_duplicate_fields(.yml, list(abstract = ""))
  .yml$abstract <- abstract

  .yml
}

#' @export
#' @rdname yml_author
yml_keywords <- function(.yml, keywords) {
  stop_if_not_all_type(keywords, "character")
  warn_if_duplicate_fields(.yml, list(keywords = ""))
  .yml$keywords <- keywords

  .yml
}

#' @export
#' @rdname yml_author
yml_subject <- function(.yml, subject) {
  stop_if_not_all_type(subject, "character")
  warn_if_duplicate_fields(.yml, list(subject = ""))
  .yml$subject <- subject

  .yml
}

#' @export
#' @rdname yml_author
yml_description <- function(.yml, description) {
  stop_if_not_all_type(description, "character")
  warn_if_duplicate_fields(.yml, list(description = ""))
  .yml$description <- description

  .yml
}

#' @export
#' @rdname yml_author
yml_category <- function(.yml, category) {
  stop_if_not_all_type(category, "character")
  warn_if_duplicate_fields(.yml, list(category = ""))
  .yml$category <- category

  .yml
}

#' @export
#' @rdname yml_author
yml_lang <- function(.yml, lang) {
  stop_if_not_all_type(lang, "character")
  warn_if_duplicate_fields(.yml, list(lang = ""))
  .yml$lang <- lang

  .yml
}


#' @export
#' @rdname yml_author
yml_toplevel <- function(.yml, ...) {
  toplevel_yml <- c(...)

  warn_if_duplicate_fields(.yml, toplevel_yml)
  .yml[names(toplevel_yml)] <- toplevel_yml

  .yml
}
