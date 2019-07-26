#' Return a blank object to be discarded from YAML
#'
#' ymlthis treats `NULL`, `NA`, and other common argument defaults as literal
#' (e.g. `author = NULL` will produce "author: null"). `yml_blank()` is a helper
#' function to indicate that the field should not be included. `yml_blank()` is
#' primarily used as a default argument for fields that should not be included
#' by default.
#'
#' @param x a field from a `yml` object
#' @return a `yml_blank` object
#' @export
#'
#' @examples
#'
#' yml() %>%
#'   yml_replace(author = yml_blank()) %>%
#'   yml_discard(~is_yml_blank(.x))
#'
#'
#' @rdname yml_blank
#' @seealso [yml_discard()], [yml_replace()]
yml_blank <- function() {
  structure(list(), class = "yml_blank")
}


#' @export
#' @rdname yml_blank
is_yml_blank <- function(x) {
  inherits(x, "yml_blank")
}

#' Write YAML field or content verbatim
#'
#' `yml_verbatim()` is a helper function to write YAML precisely as given to the
#' `yml_*()` function rather than the defaults in ymlthis and yaml. ymlthis uses
#' the yaml package to check for valid syntax; yaml and ymlthis together make
#' decisions about how to write syntax, which can often be done in numerous
#' valid ways. See [yaml::as.yaml()] for more details.
#'
#' @param x a character vector
#'
#' @return an object of class `verbatim`
#' @export
#'
#' @examples
#' # "yes" and "no" serve as alternatives to `true` and `false`. This writes
#' # "yes" literally.
#' yml_verbatim("yes")
yml_verbatim <- function(x) {
  structure(x, class = "verbatim")
}

#' Take code and write it as valid YAML
#'
#' `yml_code()` takes R code and writes it as valid YAML to be evaluated during
#' knitting. Note that `yml_code()` does not evaluate or validate the R code but
#' only captures it to use in the YAML field.
#'
#' @param x valid R code
#'
#' @return a character vector with class `verbatim`
#' @export
#'
#' @examples
#'
#' yml_code(sys.Date())
#'
#' @seealso [yml_verbatim()]
yml_code <- function(x) {
  x <- rlang::enquo(x)
  glue::glue("`r {rlang::quo_text(x)} `") %>%
    yml_verbatim()
}

#' Include content within output
#'
#' `includes2()` is a version of the `includes()` helper function from rmarkdown
#' that uses `yml_blank()` instead of `NULL` as the argument defaults, as
#' ymlthis treats NULLs as literal YAML syntax ("null").
#'
#' @param in_header	One or more files with content to be included in the header
#'   of the document.
#' @param before_body One or more files with content to be included before the
#'   document body.
#' @param after_body One or more files with content to be included after the
#'   document body.
#'
#' @return a list
#' @export
#'
#' @examples
#'
#' yml() %>%
#'   yml_output(
#'     pdf_document(includes = includes2(after_body = "footer.tex"))
#'   )
includes2 <- function(in_header = yml_blank(), before_body = yml_blank(), after_body = yml_blank()) {
  includes_list <- list(
    in_header = in_header,
    before_body = before_body,
    after_body = after_body
  )

  purrr::discard(includes_list, is_yml_blank)
}

#' Check if field exists in YAML
#'
#' `has_field()` retrieves the names of all fields (including nested fields) and
#' checks if `field` is among them.
#'
#' @template describe_yml_param
#' @param field A character vector, the name of the field(s) to check for
#'
#' @return logical
#' @export
#'
#' @examples
#'
#' has_field(yml(), "author")
#' has_field(yml(), "toc")
#'
has_field <- function(.yml, field) {
  fields <- flatten_yml_names(.yml)
  field %in% fields
}
