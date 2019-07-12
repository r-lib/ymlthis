#' Replace or discard top-level YAML fields
#'
#' `yml_replace()` replaces a named field with another value. As opposed to
#' duplicating top-level fields with other functions, explicitly replacing them
#' with `yml_replace()` will not raise a warning. `yml_discard()` removes values
#' given either a character vector of names or a purrr-style lambda with a
#' predicate (~ predicate); see the examples.
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

warn_if_duplicate_fields <- function(yml_object, new_fields) {
  fields <- names(new_fields)
  duplicate_fields <- any(names(yml_object) %in% fields)

  if (duplicate_fields) {
    fields <- glue::glue_collapse(fields, sep = ", ", last = " and ")
    msg <- glue::glue(
      "Top-level fields {fields} already present. \\
      Replacing the existing fields."
    )
    warning(msg, call. = FALSE)
  }

  invisible(yml_object)
}
