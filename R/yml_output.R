#' Title
#'
#' @param .yml
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
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

eval_with_rmarkdown <- function(x) {
  withr::with_namespace(
    "rmarkdown",
    rlang::eval_tidy(x)
  )
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

  yml_list <- list(purrr::map_if(args, rlang::is_call, eval_with_rmarkdown))
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
