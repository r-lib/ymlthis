insert_yml <- function(.yml) {
  .yml %>%
    capture_yml() %>%
    paste(collapse = "\n") %>%
    rstudioapi::insertText()
}

swap_yml <- function() {

}
