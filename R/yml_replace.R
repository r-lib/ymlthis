#' Replace or discard top-level YAML fields
#'
#' @param .yml
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
yml_replace <- function(.yml, ...) {
  new <- c(...)
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
