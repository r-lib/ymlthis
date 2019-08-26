context("test-helpers_work")
if (!rmarkdown::pandoc_available()) testthat::skip("Pandoc not found")

test_that("multiplication works", {

 x <- yml_empty() %>%
   yml_author("Malcolm Barrett") %>%
   yml_date() %>%
   yml_clean(TRUE) %>%
   yml_replace(clean = FALSE) %>%
   yml_discard("author")

 expect_equal(names(x), c("date", "clean"))
 expect_length(x, 2)

 y <- yml_empty() %>%
   yml_author("Malcolm Barrett") %>%
   yml_date() %>%
   yml_output(
     pdf_document(),
     html_document()
   ) %>%
   yml_discard(~ length(.x) > 1)

 expect_equal(names(y), c("author", "date"))
 expect_length(y, 2)
})
