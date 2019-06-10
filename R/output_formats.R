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
  args_list <- purrr::map(x, rlang::call_args)
  function_name_list <- purrr::map(x, rlang::call_name)

  #  add namespaces to functions when used
  function_namespaces <-  purrr::map(x, rlang::call_ns)
  function_name_list <- purrr::map2(
    function_name_list,
    function_namespaces,
    ~ ifelse(
      is.null(.y),
      .x,
      paste0(.y, "::", .x)
    )
  )

  if (length(x) == 1) {
    .yml$output <- parse_output_yml(args_list[[1]], function_name_list[[1]])
    return(.yml)
  }

  .yml$output <- purrr::map2(
    args_list,
    function_name_list,
    parse_output_yml,
    use_default = TRUE
  ) %>%
    purrr::flatten()

  .yml
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

  yml_list <- list(purrr::map_if(args, rlang::is_call, rlang::eval_tidy))
  names(yml_list) <- function_name

  yml_list
}
