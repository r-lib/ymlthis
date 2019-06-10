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
  stop_if_not_all_type(clean, "logical")
  .yml$clean <- clean

  .yml
}

yml_params <- function(.yml, params) {

}

yml_params_shiny <- function(.yml, params) {

}
