#' Capture, validate, and write output YAML
#'
#' `yml_output()` writes valid YAML for the `output` field of R Markdown YAML.
#' `yml_output()` captures the actual output functions, such as
#' `pdf_document()`, and translates them to YAML. This function accepts multiple
#' output formats (separated by commas) and validates each by evaluating the
#' function internally. The YAML fields in under `output` come from arguments in
#' their respective R functions. If you wanted to see the available fields in
#' `pdf_document()`, for instance, you would read the documentation for that
#' function using `?pdf_document`.
#'
#' @template describe_yml_param
#' @param ... valid R code calling functions that return objects of class
#'   `rmarkdown_output_format`, such as the `*_document()` functions in
#'   rmarkdown.
#'
#' @template describe_yml_output
#' @export
#'
#' @examples
#'
#' yml() %>%
#'   yml_output(html_document())
#'
#' yml() %>%
#'   yml_output(
#'     pdf_document(keep_tex = TRUE, includes = includes2(after_body = "footer.tex")),
#'     bookdown::html_document2()
#'   )
#'
yml_output <- function(.yml, ...) {
  x <- rlang::enquos(...)
  validate_output_yml(x)

  args_list <- purrr::map(x, rlang::call_args)
  function_name_list <- purrr::map(x, rlang::call_name)

  #  add namespaces to functions when used
  function_namespaces <-  purrr::map(x, rlang::call_ns)
  function_name_list <- purrr::map2(
    function_namespaces,
    function_name_list,
    prepend_namespace
  )


  if (length(x) == 1) {
    .yml$output <- parse_output_yml(args_list[[1]], function_name_list[[1]])
    return(.yml)
  }

  warn_if_duplicate_fields(.yml, list(output = ""))
  .yml$output <- purrr::map2(
    args_list,
    function_name_list,
    parse_output_yml,
    use_default = TRUE
  ) %>%
    purrr::flatten()

  .yml
}

eval_with_rmarkdown <- function(x, check_type = TRUE) {
  msg <- "rmarkdown must be installed to use outputs"
  if (!requireNamespace("rmarkdown", quietly = TRUE)) stop(msg, call. = FALSE)
  x <- withr::with_namespace(
    "rmarkdown",
    rlang::eval_tidy(x)
  )

  if (check_type && !inherits(x, "rmarkdown_output_format")) {
    stop(
      "`output` must return object of class `rmarkdown_output_format`",
      call. = FALSE
    )
  }

  x
}

parse_output_yml <- function(args, function_name, use_default = FALSE) {
  if (!rlang::has_length(args) && !use_default) {
    return(function_name)
  }

  if (!rlang::has_length(args) && use_default) {
    output_yml <- list("default")
    names(output_yml) <- function_name
    return(output_yml)
  }

  yml_list <- list(
    purrr::map_if(args, rlang::is_call, eval_with_rmarkdown, check_type = FALSE))
  names(yml_list) <- function_name

  yml_list
}

stop_yml_eval <- function(e, x) {
  stop(
    "Invalid argument in YAML output function ",
    rlang::quo_text(x),
    "\n",
    as.character(e),
    call. = FALSE
  )
}

eval_tidy_yml <- function(x) {
  out <- rlang::catch_cnd(
    eval_with_rmarkdown(x)
  )

  if (!is.null(out)) stop_yml_eval(out, x)

  out
}

validate_output_yml <- function(.function_calls) {
  purrr::map(.function_calls, eval_tidy_yml)
  invisible(.function_calls)
}
