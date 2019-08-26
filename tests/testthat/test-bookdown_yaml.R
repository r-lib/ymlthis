context("test-bookdown_yaml")
if (!rmarkdown::pandoc_available()) testthat::skip("Pandoc not found")
if (!requireNamespace("bookdown")) testthat::skip("bookdown not found")

test_that("multiplication works", {
  rslt <- yml_empty() %>%
    yml_author("Malcolm Barrett") %>%
    yml_date() %>%
    yml_output(
      pdf_document(keep_tex = TRUE, includes = includes2(after_body = "footer.tex")),
      bookdown::html_document2()
    ) %>%
    stringify_yaml()

  yaml_string <- "---
author: Malcolm Barrett
date: '`r format(Sys.Date())`'
output:
  pdf_document:
    keep_tex: true
    includes:
      after_body: footer.tex
  bookdown::html_document2: default
---"

  expect_equal(rslt, yaml_string)
})
