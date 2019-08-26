context("helper-utils")
library(ymlthis)
stringify_yaml <- function(x) {
  x %>%
    ymlthis:::capture_yml() %>%
    paste(collapse = "\n")
}
