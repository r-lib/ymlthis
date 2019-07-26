#' Read and write to JSON and TOML
#'
#' Read JSON and TOML files in as `yml` objects with `read_*()`. Write `yml`
#' objects out as JSON and YAML files with `write_as_*()`. These functions rely
#' on Hugo and blogdown, so you must have blogdown installed.
#'
#' @template describe_yml_param
#' @param path a path to a JSON or TOML file
#' @param out The path to write out to. If `NULL`, will write to the `path` but
#'   change the file extension to `.toml` or `.json`.
#'
#' @return a `yml` object (if reading) or the path (if writing)
#' @export
read_json <- function(path) {
  convert_metadata(path = path, to = "YAML")
}

#' @export
#' @rdname read_json
read_toml <- function(path) {
  convert_metadata(path = path, to = "YAML")
}

#' @export
#' @rdname read_json
write_as_json <- function(.yml = NULL, path = NULL, out = NULL) {
  stop_if_both_args_given(.yml, path)
  if (!is.null(.yml)) {
    path <- write_temp_yaml(.yml)
    on.exit(unlink(path))
  }

  if (is.null(out)) out <- swap_extension(path, ".json")
  convert_metadata(path = path, to = "JSON", out = out)
}

#' @export
#' @rdname read_json
write_as_toml <- function(.yml = NULL, path = NULL, out = NULL) {
  stop_if_both_args_given(.yml, path)
  if (!is.null(.yml)) {
    path <- write_temp_yaml(.yml)
    on.exit(unlink(path))
  }
  if (is.null(out)) out <- swap_extension(path, ".toml")
  convert_metadata(path = path, to = "TOML", out = out)
}

swap_extension <- function(path, ext) paste0(fs::path_ext_remove(path), ext)

stop_if_both_args_given <- function(.yml, path) {
  if (!is.null(.yml) && !is.null(path)) {
    stop(
      "You cannot specify both a `yml` object and a file to convert",
      call. = FALSE
    )
  }
}

write_temp_yaml <- function(.yml) {
  .file <- tempfile(fileext = ".yml")
  yml_txt <- yaml::as.yaml(
    .yml,
    handlers = yml_handlers(),
    column.major = FALSE
  )
  writeLines(yml_txt, .file)

  .file
}

convert_metadata <- function(path, to = c("YAML", "TOML", "JSON"), out = NULL) {
  stop_if_blogdown_not_installed()
  on.exit(unlink_temporary_dir())
  to <- match.arg(to)

  file_to_convert <- fs::path_file(path) %>%
    fs::path_ext_remove() %>%
    paste0(".md")
  file_to_convert <- file.path(temporary_dir(), "content", file_to_convert)
  file_type <- fs::path_ext(path) %>%
    tolower()

  fs::dir_create(file.path(temporary_dir(), "content"))
  rewrite_with_fences(path, file_to_convert, file_type = file_type)

  writeLines(
    c("baseurl = \"/\"", "builddrafts = true"),
    file.path(temporary_dir(), "config.toml")
  )

  withr::with_dir(
    temporary_dir(),
    blogdown::hugo_cmd(
      args = c("convert", paste0("to", to), "--unsafe"),
      stdout = TRUE
    )
  )

  if (to == "YAML") {
    post_yml <- yaml::yaml.load_file(file_to_convert) %>%
      as_yml()

    return(post_yml)
  }

  file_txt <- readLines(file_to_convert) %>%
    purrr::discard(~.x %in% c("---", "+++", "..."))

  usethis::write_over(out, file_txt)
  invisible(out)
}

rewrite_with_fences <- function(from, to, file_type) {
  fences <- switch(
    file_type,
    yml = "---",
    yaml = "---",
    toml = "+++",
    json = NULL
  )

  file_txt <- readLines(from)
  if (!is.null(fences) && file_txt[[1]] != fences) {
    file_txt <- c(fences, file_txt, fences)
  }

  writeLines(file_txt, to)
}
