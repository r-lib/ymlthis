#' Title
#'
#' @param .yml
#' @param runtime
#'
#' @return
#' @export
#'
#' @examples
yml_runtime <- function(.yml, runtime = c("static", "shiny", "shiny_prerendered")) {
  .yml$runtime <- match.arg(runtime)

  .yml
}



#' Title
#'
#' @param .yml
#' @param clean
#'
#' @return
#' @export
#'
#' @examples
yml_clean <- function(.yml, clean) {
  stop_if_not_type(clean, "logical")
  .yml$clean <- clean

  .yml
}

#' Title
#'
#' @param .yml
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
yml_params <- function(.yml, ...) {
  .yml$params <- list(...)

  .yml
}

#' Title
#'
#' @param .shiny
#'
#' @return
#' @export
#'
#' @examples
yml_params_shiny <- function(.shiny) {
  x <- rlang::enquo(.shiny)
  args <- rlang::call_args(x)
  function_name <- rlang::call_name(x)

  validate_shiny_yml(function_name, args)

  parse_shiny_yml(args, switch_shiny_param(function_name))
}

switch_shiny_param <- function(x) {
  switch(
    x,
    checkboxInput = "checkbox",
    numericInput = "numeric",
    sliderInput = "slider",
    dateInput = "date",
    textInput = "text",
    fileInput = "file",
    radioButtons = "radio",
    selectInput = "select",
    passwordInput = "password"
  )
}

parse_shiny_yml <- function(args, function_name) {
  yml_list <- purrr::map_if(args, rlang::is_call, rlang::eval_tidy)
  input_type <- list(input = function_name)
  yml_list <- c(input_type, yml_list)

  #  drop `inputId` argument if needed
  .keep <- names(yml_list) != "inputId"
  yml_list[.keep]
}

stop_yml_eval_shiny <- function(e, x) {
  stop(
    "Invalid argument in YAML Shiny parameter function ",
    paste0(x, "()"),
    "\n",
    as.character(e),
    call. = FALSE
  )
}

eval_tidy_yml <- function(.function_call, .function_name) {
  out <- rlang::catch_cnd(
    .function_call()
  )

  if (!is.null(out)) stop_yml_eval_shiny(out, .function_name)

  out
}

validate_shiny_yml <- function(.function_name, .args) {

  stop_if_not_shiny_param(.function_name)

  .function_call <- purrr::partial(
    rlang::as_function(.function_name),
    inputId = "inputId",
    !!!.args
  )
  .function_call <- eval_tidy_yml(.function_call, .function_name)
  invisible(.function_call)
}

stop_if_not_shiny_param <- function(.function_name) {

  valid_functions <- c(
    "checkboxInput",
    "numericInput",
    "sliderInput",
    "dateInput",
    "textInput",
    "fileInput",
    "radioButtons",
    "selectInput",
    "passwordInput"
  )

  if (!(.function_name %in% valid_functions)) {
    valid_functions <- valid_functions %>%
      paste0("()") %>%
      glue::glue_collapse(sep = ", ", last = ", or ")

    stop(
      "Invalid Shiny function ",
      .function_name,
      "\n",
      "Must be one of: ",
      valid_functions,
      call. = FALSE
    )
  }

  invisible(.function_name)
}
