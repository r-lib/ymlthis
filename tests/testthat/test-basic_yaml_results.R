context("test-basic_yaml_results")
if (!rmarkdown::pandoc_available()) testthat::skip()

test_that("YAML is rendered correctly", {


  rslt <- yml() %>%
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
  ymlthis:::capture_yml() %>%
  paste(collapse = "\n")

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
