if (!rmarkdown::pandoc_available()) testthat::skip("Pandoc not found")

test_that("Basic YAML is rendered correctly", {
  rslt <- yml_empty() %>%
    yml_author(
      c("Yihui Xie", "Hadley Wickham"),
      affiliation = rep("RStudio", 2)
    ) %>%
    yml_date("07/04/2019") %>%
    yml_output(
      pdf_document(
        keep_tex = TRUE,
        includes = includes2(after_body = "footer.tex")
      )
    ) %>%
    yml_latex_opts(biblio_style = "apalike") %>%
    stringify_yaml()

  yaml_string <- "---
author:
- name: Yihui Xie
  affiliation: RStudio
- name: Hadley Wickham
  affiliation: RStudio
date: 07/04/2019
output:
  pdf_document:
    keep_tex: true
    includes:
      after_body: footer.tex
biblio-style: apalike
---"
  expect_equal(rslt, yaml_string)
})

test_that("output fields render correctly", {
  rslts <- yml_empty() %>%
    yml_output(html_document()) %>%
    stringify_yaml()

  yaml_string <- "---
output: html_document
---"
  expect_equal(rslts, yaml_string)
})

test_that("LaTeX options render correctly", {
  rslt <- yml_empty() %>%
    yml_author("Malcolm Barrett") %>%
    yml_output(pdf_document()) %>%
    yml_latex_opts(
      fontfamily = "Fira Sans Thin",
      fontsize = "11pt",
      links_as_notes = TRUE
    ) %>%
    stringify_yaml()

  yaml_string <- "---
author: Malcolm Barrett
output: pdf_document
fontfamily: Fira Sans Thin
fontsize: 11pt
links-as-notes: true
---"

  expect_equal(rslt, yaml_string)
})

test_that("Code chunk is rendered correctly", {
  rslt <- code_chunk({
    yml_empty() %>%
      yml_author("Malcolm Barrett") %>%
      yml_output(pdf_document())
  }, chunk_name = "yml_example")

  expect_snapshot(rslt)
})

test_that("blanks are correctly cleared from author list", {
  author_yml <- yml_empty() %>%
  yml_distill_author(
    name = "John Doe",
    affiliation = "My Uni"
  )

  expect_snapshot(author_yml)
})

