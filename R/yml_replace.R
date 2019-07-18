#' Replace, pluck, or discard top-level YAML fields
#'
#' `yml_replace()` replaces a named field with another value. As opposed to
#' duplicating top-level fields with other functions, explicitly replacing them
#' with `yml_replace()` will not raise a warning. `yml_discard()` removes values
#' given either a character vector of names or a purrr-style lambda with a
#' predicate (~ predicate); see the examples. `yml_pluck()` and `yml_chuck()`
#' are wrappers around [purrr::pluck()] and [purrr::chuck()] that return `yml`
#' objects.
#'
#' @template describe_yml_param
#' @param .rid a character vector of fields to remove or a purrr-style lambda
#'   with a predicate (~ predicate) where fields that are `TRUE` will be
#'   discarded
#' @template describe_dots_param
#'
#' @template describe_yml_output
#' @export
#'
#' @examples
#'
#' yml() %>%
#'   yml_clean(TRUE) %>%
#'   yml_replace(clean = FALSE) %>%
#'   yml_discard("author")
#'
#' yml() %>%
#'   yml_output(
#'     pdf_document(),
#'     html_document()
#'   )%>%
#'   yml_discard(~ length(.x) > 1)
#'
#'
yml_replace <- function(.yml, ...) {
  new <- list(...)
  .yml[names(new)] <- new

  .yml
}

#' @export
#' @rdname yml_replace
yml_discard <- function(.yml, .rid) {
  if (is.character(.rid)) {
    return(
      .yml[names(.yml) %nin% .rid] %>%
        as_yml()
    )
  }

  if (is.numeric(.rid)) {
    return(
      .yml[-.rid] %>%
        as_yml()
    )
  }

  if (rlang::is_formula(.rid)) {
    return(
      purrr::discard(.yml, .rid) %>%
        as_yml()
    )
  }

  msg <- glue::glue(
  "`.rid` must be a character vector of field names \\
   or a formula specifying a predicate"
  )
  stop(msg, call. = FALSE)
}

#' @export
#' @rdname yml_replace
yml_pluck <- function(.yml, ...) {
  purrr::pluck(.yml, ..., .default = list()) %>%
    as_yml()
}

#' @export
#' @rdname yml_replace
yml_chuck <- function(.yml, ...) {
  purrr::chuck(.yml, ...) %>%
    as_yml()
}


