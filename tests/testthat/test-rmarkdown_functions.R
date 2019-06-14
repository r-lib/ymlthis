context("test-rmarkdown_functions")


if (!requireNamespace("shiny", quietly = TRUE)) skip()

expect_shiny_list <- function(x, ...) {
  expect_type(x, "list")
  expect_equal(x, list(...))
}

test_that("shiny functions return correctly", {
  checkbox_param <- shiny_checkbox(label = "checkbox")
  expect_shiny_list(checkbox_param, label = "checkbox")
  checkbox_opt <- shiny_checkbox(label = "checkbox", value = TRUE)
  expect_shiny_list(checkbox_opt, label = "checkbox", value = TRUE)

  date_param <- shiny_date(label = "date", autoclose = FALSE)
  expect_shiny_list(date_param, label = "date", autoclose = FALSE)

  file_param <- shiny_file(label = "file", multiple = TRUE)
  expect_shiny_list(file_param, label = "file", multiple = TRUE)

  numeric_param <- shiny_numeric(label = "numeric", value = 23)
  expect_shiny_list(numeric_param, label = "numeric", value = 23)

  password_param <- shiny_password(label = "password", value = "x23")
  expect_shiny_list(password_param, label = "password", value = "x23")

  radio_param <- shiny_radio(label = "radio", choices = c("yes", "no"))
  expect_shiny_list(radio_param, label = "radio", choices = c("yes", "no"))

  select_param <- shiny_select(label = "select", choices = c("yes", "no"))
  expect_shiny_list(select_param, label = "select", choices = c("yes", "no"))

  slider_param <- shiny_slider(label = "slider", min = 1, max = 0, value = .5)
  expect_shiny_list(slider_param, label = "slider", min = 1, max = 0, value = .5)

  text_param <- shiny_text(label = "text", value = "shiny text")
  expect_shiny_list(text_param, label = "text", value = "shiny text")
})
