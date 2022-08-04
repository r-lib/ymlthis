#' @importFrom utils getFromNamespace
launch_yaml_addin <- function() {
  stop_if_not_installed(c("miniUI", "shinyBS"))
  addin_dir <- system.file("addin", "new_yaml", package = "ymlthis")
  app <- shiny::shinyAppDir(addin_dir)
  shiny::runGadget(
    app,
    viewer = shiny::dialogViewer("New YAML", height = 700)
  )
}

