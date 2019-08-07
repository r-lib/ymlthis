#' @importFrom utils getFromNamespace
launch_yaml_addin <- function() {
  addin_dir <- system.file("addin", "new_yaml", package = "ymlthis")
  app <- shiny::shinyAppDir(addin_dir)
  shiny::runGadget(
    app,
    viewer = shiny::dialogViewer("New YAML")
  )
  #sys.source(system.file("addin", "new_yaml", "new_yaml.R", package = "ymlthis"))
}
