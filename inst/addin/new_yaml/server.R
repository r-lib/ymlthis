# Server ------------------------------------------------------------------
library(shiny)
library(miniUI)

 shiny::shinyServer(function(input, output, session){
   shiny::observe({
     if (input$export_file %in% c("YAML", "R Markdown")) {
       file_name <- ifelse(input$export_file == "YAML", "_output.yml", "Untitled.Rmd")
       shiny::updateTextInput(session, "file_path", value = file_name)
     }
   })

   params_handlers <- shiny::reactiveValues(
     params = list(),
     value = list(),
     input = list(),
     label = list(),
     created = list()
   )
   params_observers <- shiny::reactiveValues(observers = list())

   shiny::observeEvent(
     input$add_param, {
       shiny::req(input$param)
       shiny::req(input$param_value)
       if (!is.null(params_handlers$params[[input$param]])) {
         shiny::removeUI(glue::glue("#param_{input$param}"))
         shiny::removeUI(glue::glue("#modal_param_{input$param}"))
         params_observers$observers[[input$param]]$destroy()
         for (x in names(params_handlers)) params_handlers[[x]][[input$param]] <- NULL
       }

       selector <- ifelse(
         purrr::is_empty(params_handlers$params),
         "holder",
         glue::glue("{params_handlers$params[[length(params_handlers$params)]]}")
       )

       shiny::insertUI(
         selector = glue::glue("#param_{selector}"),
         where = "afterEnd",
         ui = ui_param(
           input$param,
           input$param_value,
           stringr::str_remove(input$shiny_fun, "shiny_"),
           input$param_label
         )
       )

       if (!is.null(input$shiny_fun) && input$shiny_fun != "") {
         shiny::insertUI(
           selector = "#modal_holder",
           where = "afterEnd",
           ui = output_modal(
             shiny_switch(input$shiny_fun),
             modal_name = glue::glue("param_{input$param}"),
             title = "Input options",
             id = glue::glue("modal_param_{input$param}"),
             ns = "shiny"
           )
         )
       }
       purrr::walk(
         c("param", "param_value", "param_label"),
         shiny::updateTextInput,
         value = "",
         session = session
       )

       params_handlers$params[[input$param]] <- input$param
       params_handlers$value[[input$param]] <- input$param_value
       params_handlers$input[[input$param]] <- input$shiny_fun
       params_handlers$label[[input$param]] <- ifelse(
         input$param_label == "" | purrr::is_empty(input$param_label),
         input$param, input$param_label
       )
     })


   shiny::observeEvent(params_handlers$params, {
     for (i in seq_along(params_handlers$params)) {
       param <- params_handlers$params[[i]]
       rmv_button <- glue::glue("remove_param_{param}")
       if (is.null(params_handlers$created[[param]])) {
         params_observers$observers[[param]] <- shiny::observeEvent(
           input[[rmv_button]], {
             shiny::removeUI(glue::glue("#param_{param}"))
             params_handlers$created[[param]] <- NULL
             params_handlers$params[[param]] <- NULL
           },
           ignoreInit = TRUE,
           once = TRUE
         )

         params_handlers$created[[param]] <- TRUE
       }
     }

   })

  shiny::observeEvent(input$done, {
     input_date <- swap_arg(input$date)
     if (input$use_date) input_date <- NULL

     shiny_yml <- yml_empty() %>%
       yml_author(swap_arg(input$author)) %>%
       yml_date(input_date) %>%
       yml_title(swap_arg(input$title)) %>%
       yml_subtitle(swap_arg(input$subtitle)) %>%
       capture_output_functions(input) %>%
       capture_params(params_handlers, input) %>%
       yml_keywords(swap_arg(input$keywords)) %>%
       yml_subject(swap_arg(input$subject)) %>%
       yml_category(swap_arg(input$category)) %>%
       yml_abstract(swap_arg(input$abstract)) %>%
       yml_description(swap_arg(input$description)) %>%
       yml_lang(swap_arg(input$lang)) %>%
       yml_resource_files(swap_arg(input$resource_files$name)) %>%
       yml_clean(swap_arg(input$clean, .default = TRUE)) %>%
       pass_if(
         is_yml_blank(swap_arg(input$runtime)),
         yml_runtime,
         swap_arg(input$runtime)
       ) %>%
       yml_latex_opts(
         fontsize = swap_arg(input$fontsize),
         fontfamily = swap_arg(input$fontfamily),
         linkcolor = swap_arg(input$linkcolor),
         citecolor = swap_arg(input$citecolor),
         urlcolor = swap_arg(input$urlcolor),
         documentclass = swap_arg(input$documentclass),
         classoption = swap_arg(input$classoption),
         indent = swap_arg(input$indent, .default = FALSE),
         geometry = swap_arg(input$geometry),
         links_as_notes = swap_arg(input$links_as_notes, .default = FALSE),
         lof = swap_arg(input$lof, .default = FALSE),
         lot = swap_arg(input$lot, .default = FALSE),
         thanks = swap_arg(input$thanks),
         biblio_title = swap_arg(input$biblio_title),
         biblio_style = swap_arg(input$biblio_style),
         natbiboptions = swap_arg(input$natbiboptions),
         biblatexoptions = swap_arg(input$biblatexoptions)
       ) %>%
       yml_citations(
         bibliography = swap_arg(input$bibliography$name),
         csl = swap_arg(input$csl$name),
         link_citations = swap_arg(input$link_citations, .default = FALSE),
         suppress_bibliography = swap_arg(input$suppress_bibliography, .default = FALSE)
       ) %>%
       yml_discard(~is_yml_blank(.x))

     export_f <- switch(
       input$export_file,
       "R Markdown" = use_rmarkdown,
       "YAML" = use_yml_file,
       "Place YAML on clipboard" = use_yml
     )

     if (input$export_file == "R Markdown") {
       path <- input$file_path
       template <- input$rmd_template$name
       export_f <- purrr::partial(export_f, path = path, template = template)
     }

     if (input$export_file == "YAML") {
       path <- input$file_path
       export_f <- purrr::partial(export_f, path = path)
     }

    shiny::onStop(function() export_f(shiny_yml))
    shiny::stopApp()
  })
})
