#' Create a new yml object
#'
#' `yml()` initializes a `yml` object. `yml` objects create valid YAML and print
#' it cleanly to the console. By default, `yml()` looks for your name (using `
#' getOption("usethis.full_name")`, `getOption("devtools.name")`, and
#' `whoami::fullname()`) and uses today's date to use in the `author` and `date`
#' fields, respectively. If you've set default YAML in
#' `getOption("ymlthis.default_option")` (see [use_yml_defaults()]), `yml()`
#' will also use include those fields by default. `yml_empty()` is a wrapper
#' that doesn't use any of these default YAML fields. `yml()` and all
#' related`yml_*()` functions validate that the results are indeed valid YAML
#' syntax, although not every function is able to check that the input fields
#' are valid for the setting they are used in.
#'
#' @details
#'
#' `.yml` accepts a character vector of YAML, such as "author: Hadley Wickham",
#' an object returned by ymlthis functions that start with `yml_*()`, or a
#' `list` object (e.g. `list(author = "Hadley Wickham")`). `.yml` objects are
#' processed with [`as_yml()`], a wrapper around [`yaml::yaml.load()`]. See that
#' function for more details.
#'
#' @param .yml a character vector, `yml` object, or YAML-like list. See details.
#' @param get_yml logical. Use YAML stored in
#'   `getOption("ymlthis.default_option")`? By default, `yml()` includes if it
#'   exists.
#' @param author logical. Get default author name?
#' @param date logical. Get default date?
#'
#' @template describe_yml_output
#' @export
#'
#' @examples
#'
#' yml()
#'
#' yml(date = FALSE)
#'
#' "author: Hadley Wickham\ndate: 2014-09-12" %>%
#'   yml() %>%
#'   yml_title("Tidy Data") %>%
#'   yml_keywords(
#'     c("data cleaning", "data tidying", "relational databases", "R")
#'   )
#'\donttest{
#' yml() %>%
#'   yml_author(
#'     c("Yihui Xie", "Hadley Wickham"),
#'     affiliation = rep("RStudio", 2)
#'   ) %>%
#'   yml_date("07/04/2019") %>%
#'   yml_output(
#'     pdf_document(
#'     keep_tex = TRUE,
#'     includes = includes2(after_body = "footer.tex")
#'    )
#'   ) %>%
#'   yml_latex_opts(biblio_style = "apalike")
#'}
yml <- function(.yml = NULL, get_yml = TRUE, author = TRUE, date = TRUE) {
  if (is.null(.yml)) .yml <- list()
  .yml <- as_yml(.yml)
  if (!is.null(.yml$author)) author <- FALSE
  if (!is.null(.yml$date)) date <- FALSE

  if (get_yml) default_yml <- get_yml_defaults()
  if (get_yml && !is.null(default_yml)) {
    default_fields <- names(default_yml)
    if ("author" %in% default_fields) author <- FALSE
    if ("date" %in% default_fields) date <- FALSE
    .yml[default_fields] <- default_yml
  }

  if (author) {
    author_name <- tryCatch(
      get_author_name(),
      error = function(e) yml_blank()
    )
    if (!is_yml_blank(author_name)) .yml$author <- author_name
  }
  if (date) .yml$date <- format_sys_date()

  .yml
}


#' @rdname yml
#' @export
yml_empty <- function() {
  yml(get_yml = FALSE, author = FALSE, date = FALSE)
}

#' Convert to yml object
#'
#' `as_yml` is a wrapper for [`yaml::yaml.load()`] that stores YAML as a `yml`
#' object, which prints cleanly to the console and is easy to work with using
#' ymlthis functions.
#'
#' @param x An object, either a character vector of length 1 or list, to convert
#'   to `yml`.
#'
#' @template describe_yml_output
#' @export
#'
#' @examples
#'
#' x <- as_yml("
#'   author: Hadley Wickham
#'   date: '2014-09-12'
#'   title: Tidy Data
#'   keywords:
#'   - data cleaning
#'   - data tidying
#'   - relational databases
#'   - R")
#'
#'   x
#'
#'   x %>%
#'     yml_subtitle("Hadley's Tidy Data Paper")
#'
as_yml <- function(x) {
  UseMethod("as_yml")
}

#' @export
as_yml.default <- function(x) {
  yaml::as.yaml(x) %>%
    as_yml()
}

#' @export
as_yml.list <- function(x) {
  structure(
    x,
    class = "yml"
  )
}

#' @export
as_yml.character <- function(x) {
  if (length(x) == 1) {
    .yml <- yaml::yaml.load(x)
    if (is.character(.yml)) return(.yml)
    return(as_yml(.yml))
  }

  structure(
    x,
    class = "yml"
  )
}

#' @export
as_yml.print_yaml <- function(x) {
  .yml <- unclass(x)
  as_yml(.yml)
}

#' Load YAML from string
#'
#' `yml_load()` is a wrapper for [yaml::yaml.load()] that also converts the
#' object to the `yml` class.
#'
#' @param x an object to pass to [yaml::yaml.load()]
#'
#' @examples
#' c("title: my title", "author: Malcolm Barrett") %>%
#'   yml_load()
#'
#' @export
yml_load <- function(x) {
  as_yml(yaml::yaml.load(x))
}

#' Set handlers to process the way YAML is printed
#'
#' ymlthis uses the yaml package to process and validate YAML; this package also
#' lets you specify how fields and values are printed using a list of handler
#' functions. `yml_handlers()` specifies defaults for the package used in the
#' print statement. See [yaml::yaml.load()] for more on specifying handlers.
#'
#' @export
yml_handlers <- function() {
  list(
    NULL = function(x) yml_verbatim("null"),
    glue = function(x) as.character(x),
    Date = function(x) as.character(x),
    logical = function(x) yml_verbatim(ifelse(x, "true", "false"))
  )
}

#' @export
print.yml <- function(x, ..., handlers = yml_handlers()) {
  #  save to be grabbed by last_yml()
  .ymlthis$.yml <- x
  yml_txt <- color_yml(x, handlers = handlers)

  cat_silver("---\n")
  cat(yml_txt, ...)
  cat_silver("---\n")

  invisible(x)
}

color_yml <- function(x, handlers = yml_handlers()) {
  yml_txt <- yaml::as.yaml(
    x,
    handlers = handlers,
    column.major = FALSE
  )

  field_names <- x %>%
    flatten_yml_names() %>%
    paste(collapse = "|")

  # start with `-`, spaces, or beginning line, and end with a colon
  field_names <- paste0("(?:(?<=[- ])|^)(", field_names, ")(?=:)")

  yml_txt <- yml_txt %>%
    #  color fields green
    split_pluck() %>%
    stringr::str_replace(field_names, crayon::green) %>%
    paste(collapse = "\n") %>%
    # color list hyphens and single colons silver
    stringr::str_replace_all("-\\s", crayon::silver) %>%
    stringr::str_replace_all("(?<!\\:)\\:(?!\\:)", crayon::silver)

  yml_txt
}

flatten_yml_names <- function(x) {
  seq(from = 0, to = purrr::vec_depth(x) - 1) %>%
    purrr::map(~purrr::map_depth(x, .x, names, .ragged = TRUE)) %>%
    unlist(use.names = FALSE)
}

cat_silver <- function(x) {
  cat(crayon::silver(x))
}

#' Export yml object as a YAML knitr code chunk
#'
#' `asis_yaml_output()` exports a `yml` object as a YAML knitr code chunk
#' instead of as an R object. Doing so adds code highlighting for YAML syntax.
#'
#' @template describe_yml_param
#' @param fences Logical. Write fences ("---") before and after YAML?
#'
#' @export
asis_yaml_output <- function(.yml, fences = TRUE) {
  x <- .yml %>%
    capture_yml()
  if (!fences) x <- x[which(x != "---")]
  x <- glue::glue_collapse(x, "\n")

  glue::glue("```yaml\n{x}\n```") %>%
    knitr::asis_output()
}

#' Is object a yml object?
#'
#' @param x An object to test
#'
#' @return A logical vector
#' @export
is_yml <- function(x) inherits(x, "yml")

#  set environment to store last yml
.ymlthis <- new.env(parent = emptyenv())
.ymlthis$.yml <- list()

#' Return the most recently printed YAML
#'
#' ymlthis stores the most recently printed `yml` object; you can use
#' `last_yml()` to retrieve it to modify, pass to `use_*()` functions, and so
#' on.
#'
#' @export
#'
#' @examples
#' yml() %>%
#'   yml_author("Yihui Xie")
#'
#' last_yml()
#'
last_yml <- function() {
  if (rlang::is_empty(.ymlthis$.yml)) .ymlthis$.yml <- yml()
  .ymlthis$.yml
}
