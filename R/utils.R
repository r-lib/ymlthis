stop_if_not_type <- function(x, type) {
  if (!is(x, type)) {
    x_text <- rlang::quo_text(rlang::quo(x))
    stop_must_be_type(x_text, type)
  }

  invisible(x)
}

stop_if_not_all_type <- function(x, type) {
  x_text <- rlang::quo_text(rlang::quo(x))
  all_type <- all(purrr::map_lgl(x, is, type))
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

capture_yml <- function(.yml) {
  capture.output(print(.yml))
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
