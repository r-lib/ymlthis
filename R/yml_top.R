#' Title
#'
#' @param .yml
#' @param name
#' @param affiliation
#'
#' @return
#' @export
#'
#' @examples
yml_author <- function(.yml, name = NULL, affiliation = NULL) {
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
    .yml$author <- purrr::map2(name, affiliation, ~author_list(.x, .y))
    return(.yml)
  }

  warn_if_duplicate_fields(.yml, list(author = ""))
  .yml$author <- get_author_name()

  .yml
}

author_list <- function(.x, .y) {
  if (is.na(.x)) return(list(affiliation = .y))
  if (is.na(.y)) return(list(name = .x))

  list(name = .x, affiliation = .y)
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

#' Title
#'
#' @param .yml
#' @param date
#' @param format
#'
#' @return
#' @export
#'
#' @examples
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

#' Title
#'
#' @param .yml
#' @param title
#' @param subtitle
#'
#' @return
#' @export
#'
#' @examples
#' @rdname yml_title
yml_title <- function(.yml, title) {
  stop_if_not_type(title, "character")
  warn_if_duplicate_fields(.yml, list(title = ""))
  .yml$title <- title

  .yml
}

#' @export
#' @rdname yml_title
yml_subtitle <- function(.yml, subtitle) {
  stop_if_not_type(subtitle, "character")
  warn_if_duplicate_fields(.yml, list(subtitle = ""))
  .yml$subtitle <- subtitle

  .yml
}

#' Title
#'
#' @param .yml
#' @param abstract
#'
#' @return
#' @export
#'
#' @examples
#' @rdname yml_top_description
yml_abstract <- function(.yml, abstract) {
  stop_if_not_type(abstract, "character")
  warn_if_duplicate_fields(.yml, list(abstract = ""))
  .yml$abstract <- abstract

  .yml
}

#' Title
#'
#' @param .yml
#' @param keywords
#'
#' @return
#' @export
#'
#' @examples
yml_keywords <- function(.yml, keywords) {
  stop_if_not_all_type(keywords, "character")
  warn_if_duplicate_fields(.yml, list(keywords = ""))
  .yml$keywords <- keywords

  .yml
}

#' Title
#'
#' @param .yml
#' @param subject
#'
#' @return
#' @export
#'
#' @examples
yml_subject <- function(.yml, subject) {
  stop_if_not_all_type(subject, "character")
  warn_if_duplicate_fields(.yml, list(subject = ""))
  .yml$subject <- subject

  .yml
}

#' Title
#'
#' @param .yml
#' @param description
#'
#' @return
#' @export
#'
#' @examples
yml_description <- function(.yml, description) {
  stop_if_not_all_type(description, "character")
  warn_if_duplicate_fields(.yml, list(description = ""))
  .yml$description <- description

  .yml
}

#' Title
#'
#' @param .yml
#' @param category
#'
#' @return
#' @export
#'
#' @examples
#' @rdname yml_top_description
yml_category <- function(.yml, category) {
  stop_if_not_all_type(category, "character")
  warn_if_duplicate_fields(.yml, list(category = ""))
  .yml$category <- category

  .yml
}

#' Title
#'
#' @param .yml
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
yml_toplevel <- function(.yml, ...) {
  toplevel_yml <- c(...)

  warn_if_duplicate_fields(.yml, toplevel_yml)
  .yml[names(toplevel_yml)] <- toplevel_yml

  .yml
}
