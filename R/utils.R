stop_if_not_installed <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      pkg,
      " must be installed to use this function.\n",
      "install.packages('",
      pkg,
      "')"
    )
  }

  invisible()
}

stop_if_shiny_not_installed <- function() {
  stop_if_not_installed("shiny")
}

stop_if_rmarkdown_not_installed <- function() {
  stop_if_not_installed("rmarkdown")
}

stop_if_blogdown_not_installed <- function() {
  stop_if_not_installed("blogdown")
}

stop_if_not_type <- function(x, type) {
  if (is_yml_blank(x)) return(invisible(x))
  if (!inherits(x, type)) {
    x_text <- rlang::quo_text(rlang::quo(x))
    stop_must_be_type(x_text, type)
  }

  invisible(x)
}

stop_if_not_all_type <- function(x, type) {
  x_text <- rlang::quo_text(rlang::quo(x))
  all_type <- all(purrr::map_lgl(x, inherits, type))
  if (!all_type)  {
    x_text <- rlang::quo_text(rlang::quo(x))
    stop_must_be_type(x_text, type)
  }
}

stop_must_be_type <- function(x, type) {
  usethis::ui_stop(
    "{usethis::ui_code(x)} must be of type {usethis::ui_code(type)}"
  )
}

warn_if_duplicate_fields <- function(yml_object, new_fields) {
  fields <- names(new_fields)
  duplicate_fields <- any(names(yml_object) %in% fields)

  if (duplicate_fields) {
    fields <- glue::glue("`{fields}`") %>%
      glue::glue_collapse(sep = ", ", last = " and ")
    msg <- glue::glue(
      "Top-level fields {fields} already present. \\
      Replacing the existing fields. \\
      Use `yml_replace() if you want to replace fields without a warning."
    )
    warning(msg, call. = FALSE)
  }

  invisible(yml_object)
}

capture_yml <- function(.yml) {
  withr::local_envvar(NO_COLOR = TRUE)
  utils::capture.output(print(.yml))
}

split_pluck <- function(string) {
  x <- stringr::str_split(string, "\n")
  x[[1]]
}

prepend_namespace <- function(function_namespace, function_name) {
  ifelse(
    is.null(function_namespace),
    function_name,
    paste0(function_namespace, "::", function_name)
  )
}

`%nin%` <- Negate("%in%")

# These are derived from https://github.com/r-lib/cli/blob/e9acc82b0d20fa5c64dd529400b622c0338374ed/R/tree.R#L111
box_chars <- function(.subset = NULL) {
  if (is_utf8_output()) {
    x <- list(
      "h" = "\u2500",                   # horizontal
      "v" = "\u2502",                   # vertical
      "l" = "\u2514",
      "j" = "\u251C"
    )
  } else {
    x <- list(
      "h" = "-",                        # horizontal
      "v" = "|",                        # vertical
      "l" = "\\",
      "j" = "+"
    )
  }

  if (!is.null(.subset)) {
    return(x[[.subset]])
  }

  x
}

is_latex_output <- function() {
  if (!("knitr" %in% loadedNamespaces())) return(FALSE)
  get("is_latex_output", asNamespace("knitr"))()
}

is_utf8_output <- function() {
  opt <- getOption("cli.unicode", NULL)
  if (!is.null(opt)) {
    isTRUE(opt)
  } else {
    l10n_info()$`UTF-8` && !is_latex_output()
  }
}

temporary_dir <- function() {
  tmp_dir_path <- file.path(tempdir(), "ymlthis_temp_dir__")
  if (!fs::dir_exists(tmp_dir_path)) {
    fs::dir_create(tmp_dir_path)
  }

  tmp_dir_path
}

unlink_temporary_dir <- function() {
  tmp_dir_path <- file.path(tempdir(), "ymlthis_temp_dir__")
  unlink(tmp_dir_path, recursive = TRUE, force = TRUE)
  invisible(tmp_dir_path)
}
