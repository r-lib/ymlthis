# Resources ---------------------------------------------------------------
library(ymlthis)
shiny::addResourcePath("sbs", system.file("www", package = "shinyBS"))

# Functions ---------------------------------------------------------------
author_name <- function() {
 tryCatch(
   ymlthis:::get_author_name(),
   error = function(e) ""
 )
}

args_as_char <- function(x) {
 if (is.null(x)) return("NULL")
 if (is.call(x)) return(rlang::quo_text(x))
 as.character(x)
}

arg_textInput <- function(arg_name, arg_val, f_name, id =  NULL) {
 if (is.null(id)) id <- f_name
 input_id <- glue::glue("{id}_{arg_name}")
 ph <- args_as_char(arg_val)
 arg_name <- glue::glue("<code>{arg_name}</code>")
 shiny::textInput(input_id, shiny::HTML(arg_name), placeholder = ph)
}

ui_function_args <- function(f_name, id = NULL, ns = "rmarkdown") {
 args <- rlang::fn_fmls(utils::getFromNamespace(f_name, ns))
 if (ns == "shiny") args[c("inputId", "label", "value")] <- NULL
 tags <- purrr::map2(
   names(args),
   args,
   arg_textInput,
   f_name = f_name,
   id = id
 )
 shiny::tagList(tags)
}

output_buttons <- function(f_name, short_name) {
 if (is.na(f_name)) return(NULL)
 id <- glue::glue("button_{f_name}")
 txt <- glue::glue("Set {short_name} options")
 shiny::conditionalPanel(
   condition = glue::glue("input.output_function.includes('{f_name}')"),
   shiny::actionButton(id, txt, style = "margin-top:4px; margin-right:2px"),
   style = "display:inline-block"
 )
}

output_modal <- function(f_name, modal_name = NULL, title = "Output options", id = NULL, ns = "rmarkdown") {
 if (is.null(modal_name)) modal_name <- f_name
 shinyBS::bsModal(
   glue::glue("modal_{modal_name}"),
   glue::glue("{title}: {f_name}"),
   glue::glue("button_{modal_name}"),
   ui_function_args(f_name, id = id, ns = ns),
   size = "small"
 )
}

ui_output_action_buttons <- function(x) {
 tags <- purrr::map2(x, names(x), output_buttons)
 shiny::tagList(tags)
}

ui_output_modals <- function(x) {
 tags <- purrr::map(x, output_modal)
 shiny::tagList(tags)
}

param_label <- function(label, x) {
 if (x == "" || is.null(x)) return(NULL)
 p_html <- "<code style = 'color:black;background-color:#F0F0F0;'><label>{label}:</label>{x}</code>"
 shiny::div(shiny::HTML(glue::glue(p_html)))
}

ui_param <- function(param, value, shiny_function, label) {
 input_button <- shiny::actionButton(
   glue::glue("button_param_{param}"),
   glue::glue("options")
 )
 if ((shiny_function == "" || is.null(shiny_function))) input_button <- NULL
 param_row <- shiny::fillRow(
   param_label("param", param),
   param_label("value", value),
   param_label("label", label),
   input_button,
   shinyBS::bsButton(glue::glue("remove_param_{param}"), "Remove", style = "danger"),
   height = 70
 )
 shiny::tags$div(param_row, id = glue::glue("param_{param}"))
}

rmarkdown_outputs <- c(
 "html" = "html_document",
 "pdf" = "pdf_document",
 "word" = "word_document",
 "odt" = "odt_document",
 "rtf" = "rtf_document",
 "md" = "md_document",
 "ioslides" = "ioslides_presentation",
 "slidy" = "slidy_presentation",
 "beamer" = "beamer_presentation",
 "powerpoint" = "powerpoint_presentation"
)

shiny_functions <- c(
 "",
 "checkbox" = "shiny_checkbox",
 "date" = "shiny_date",
 "file" = "shiny_file",
 "numeric" = "shiny_numeric",
 "password" = "shiny_password",
 "radio" = "shiny_radio",
 "select" = "shiny_select",
 "slider" = "shiny_slider",
 "text" = "shiny_text"
)

shiny_switch <- function(x) {
 switch(
   x,
   "shiny_checkbox" = "checkboxInput",
   "shiny_numeric" = "numericInput",
   "shiny_slider" = "sliderInput",
   "shiny_date" = "dateInput",
   "shiny_text" = "textInput",
   "shiny_file" = "fileInput",
   "shiny_radio" = "radioButtons",
   "shiny_select" = "selectInput",
   "shiny_password" = "passwordInput"
 )
}

swap_arg <- function(x, .default = NULL) {
 if (purrr::is_empty(x) || x == "") return(yml_blank())
 if (identical(.default, x)) return(yml_blank())
 x
}

pass_if <- function(x, pred, .f, ...) {
 if (pred) return(x)
 .f(x, ...)
}

input_starts_with <- function(input, .match) {
 matches <- names(input)[startsWith(names(input), .match)]
 matched_inputs <- purrr::map(matches, ~input[[.x]])
 names(matched_inputs) <- matches
 matched_inputs
}

capture_arg <- function(x) {
 if (x == "" || stringr::str_detect(x, "[\"\']{2}")) return(x)
 arg_guess <- type.convert(x, as.is = TRUE)
 if (is.character(arg_guess)) arg_guess <- glue::glue("\"{arg_guess}\"")

 arg_guess
}

parse_arguments <- function(x) {
 if (purrr::is_empty(x) || x == "") return("")
 args <- purrr::map2_chr(x, names(x), ~glue::glue("{.y} = {capture_arg(.x)}"))
 glue::glue_collapse(args, ", ")
}
parse_dots <- function(fn_args) {
 dot_col <- stringr::str_detect(names(fn_args), "\\.\\.\\.")
 if (!any(dot_col)) return(fn_args)
 dot_txt <- fn_args[[which(dot_col)]]
 dot_list <- glue::glue("list({dot_txt})") %>%
   rlang::parse_expr() %>%
   rlang::eval_tidy()
 x <- fn_args[!dot_col]
 x[names(dot_list)] <- dot_list
 x
}

parse_output <- function(input, .f, .match = NULL, value = NULL, label = NULL) {
 if (is.null(.match)) .match <- .f
 fn_args <- input_starts_with(input, .match) %>%
   purrr::map(swap_arg) %>%
   purrr::discard(is_yml_blank) %>%
   parse_dots()
 fn_args <- fn_args[names(fn_args) != .match]
 names(fn_args) <- stringr::str_remove_all(names(fn_args), glue::glue("{.match}_?"))
 if (.f %in% shiny_functions) {
   fn_args <- c(value = value, label = label, fn_args)
 }
 .call <- glue::glue("{.f}({parse_arguments(fn_args)})")
 .call
}

capture_output_functions <- function(.yml, input) {
 .fs <- input$output_function
 if (purrr::is_empty(.fs) || .fs == "") return(.yml)
 fn_calls <- purrr::map_chr(.fs, parse_output, input = input) %>%
   glue::glue_collapse(sep = ", ")
 glue::glue(".yml %>% yml_output({fn_calls})") %>%
   rlang::parse_expr() %>%
   rlang::eval_tidy()
}

parse_param <- function(param, value, shiny_input, input, label) {
 if (is_yml_blank(swap_arg(shiny_input))) {
   return(glue::glue("{param} = {capture_arg(value)}"))
 }
 .match <- glue::glue("modal_param_{param}")
 shiny_call <- parse_output(
   input,
   shiny_input,
   .match = .match,
   value = value,
   label = label
 )
 glue::glue("{param} = {shiny_call}")
}

capture_params <- function(.yml, params_handlers, input) {
 if (purrr::is_empty(params_handlers$params)) {
   return(.yml)
 }
 index <- c("params", "value", "input", "label")
 rv_list <- shiny::reactiveValuesToList(params_handlers)[index]
 names(rv_list) <- c("param", "value", "shiny_input", "label")
 param_list <- purrr::pmap_chr(
   rv_list,
   parse_param,
   input = input
 ) %>%
   glue::glue_collapse(sep = ", ")
 glue::glue(".yml %>% yml_params({param_list})") %>%
   rlang::parse_expr() %>%
   rlang::eval_tidy()
}
