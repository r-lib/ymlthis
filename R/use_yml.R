#' Title
#'
#' @param .yml
#'
#' @return
#' @export
#'
#' @examples
use_yml <- function(.yml = NULL) {
  if (!is.null(.yml)) {
    return(return_yml_code(.yml))
  }

  # find existing YAML options
  .yml <- get_yml_defaults()
  if (!is.null(.yml)) {
    return(return_yml_code(.yml))
  }


  #  stop if no YAML found
  usethis::ui_stop(
    "`{usethis::ui_code(.yml)}` must be specified or \\
     default `{usethis::ui_code('ymlthis')}` options must \\
     be set. See `{usethis::ui_code(?use_yml_defaults())}`"
  )
}

return_yml_code <- function(.yml) {
  yaml_text <- capture_yml(.yml)
  usethis::ui_code_block(yaml_text)
  usethis::ui_todo("Paste into R Markdown or YAML file")

  invisible(.yml)
}

use_output_yml <- function(x = NULL, path = ".") {
  fs::file_create(file.path(path, "_output.yml"))
}


use_yml_defaults <- function(.yml) {
  if (!is_yml(.yml) && !is.character(.yml)) {
    usethis::ui_stop(
      "`{usethis::ui_code(.yml)}` must be a `{usethis::ui_code(yml)}` \\
      object or a `{usethis::ui_code(character)}` vector containing \\
      valid YAML text"
    )
  }

 if (is.character(.yml)) .yml <- as_yml(.yml)

 .yml_text <- capture_yml(.yml)
 .yml_text <- glue::glue_collapse(.yml, sep = "\n")
 .yml_code <- glue::glue("options(ymlthis.default_yaml = {.yml_text})")

 usethis::ui_code_block(.yml_code)
 usethis::ui_todo(
   "Run interactively or paste into .Rprofile //
   (perhaps using {usethis::ui_code('usethis::edit_r_profile()')})"
 )

 invisible(.yml)
}

get_yml_defaults <- function() {
  .yml <- getOption("ymlthis.default_yml")

  if (is.character(.yml)) .yml <- yaml::yaml.load(.yml)

  .yml
}

raw_yml <- function(x) {
  if (isTRUE(getOption("knitr.in.progress"))) return(knitr::asis_output(x))
  x
}
