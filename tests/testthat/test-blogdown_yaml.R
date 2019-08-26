context("test-blogdown_yaml")
if (!rmarkdown::pandoc_available()) testthat::skip("Pandoc not found")
if (!requireNamespace("blogdown")) testthat::skip("blogdown not found")

test_that("multiplication works", {
  output_yml <- yml_empty() %>%
  yml_output(
    bookdown::gitbook(
      lib_dir = "assets",
      split_by = "section",
      config = gitbook_config(toolbar_position = "static")
    ),
    bookdown::pdf_book(keep_tex = TRUE),
    bookdown::html_book(css = "toc.css")
  ) %>%
    stringify_yaml()

  yaml_string <- "---
output:
  bookdown::gitbook:
    lib_dir: assets
    split_by: section
    config:
      toolbar:
        position: static
  bookdown::pdf_book:
    keep_tex: true
  bookdown::html_book:
    css: toc.css
---"

  expect_equal(output_yml, yaml_string)
})
