#' @importFrom utils getFromNamespace
launch_yaml_addin <- function() {
  req_pkgs <- c("shiny", "miniUI", "shinyBS")
  not_installed <- purrr::map_lgl(req_pkgs, Negate(requireNamespace), quietly = TRUE)
  if (any(not_installed)) {
    req_pkgs <- req_pkgs[not_installed]
    cat_pkgs <- glue::glue_collapse(req_pkgs, sep = ", ")
    error_msg <- glue::glue("Required packages not found: {cat_pkgs}")
    info_msg <- glue::glue(
      "Using the add-in requires the following packages that \\
      are not installed: {cat_pkgs}."
    )
    pkg_string <- glue::glue("'{req_pkgs}'") %>%
      glue::glue_collapse(", ")
    if (sum(not_installed) > 1) pkg_string <- glue::glue("c({pkg_string})")
    install_string <- glue::glue("install.packages({pkg_string})")
    on.exit({
      usethis::ui_info(info_msg)
      usethis::ui_todo("install them with {usethis::ui_code(install_string)}")
    })
    stop(error_msg, call. = FALSE)
  }
  addin_dir <- system.file("addin", "new_yaml", package = "ymlthis")
  app <- shiny::shinyAppDir(addin_dir)
  shiny::runGadget(
    app,
    viewer = shiny::dialogViewer("New YAML")
  )
  #sys.source(system.file("addin", "new_yaml", "new_yaml.R", package = "ymlthis"))
}
