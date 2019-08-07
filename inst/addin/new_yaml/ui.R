library(shiny)
library(miniUI)

# YAML panel --------------------------------------------------------------
main_yaml_panel <- miniUI::miniTabPanel(
 "YAML",
 icon = shiny::icon("sliders"),
 miniUI::miniContentPanel(
   shiny::fillRow(
     shiny::textInput("author", "Author", value = author_name(), width = "95%"),
     shiny::fillCol(
       shiny::dateInput("date", "Date", value = "`r Sys.Date()`", width = "95%"),
       shiny::checkboxInput("use_date", "Use system date", value = TRUE)
     ),
     height = 100
   ),
   shiny::fillRow(
     shiny::textInput("title", "Title", value = "Untitled", width = "95%"),
     shiny::textInput("subtitle", "Subtitle", width = "95%"),
     height = 70
   ),
   shiny::fillRow(
     shiny::selectizeInput(
       "output_function",
       "Output",
       choices = rmarkdown_outputs,
       selected = "html_document",
       multiple = TRUE,
       width = "95%"
     ),
     miniUI::miniContentPanel(
       ui_output_action_buttons(rmarkdown_outputs),
       padding = 4
     ),
     height = 120
   ),
   ui_output_modals(rmarkdown_outputs),
   shiny::fillRow(
     shiny::fillCol(
       shiny::selectizeInput(
         "export_file",
         "Export to",
         choices = c(
           "R Markdown",
           "YAML",
           "Place YAML on clipboard"
         ),
         width = "95%"
       ),
       shiny::conditionalPanel(
         condition = "input.export_file == 'R Markdown'",
         shiny::fileInput(
           "rmd_template",
           "R Markdown Template",
           placeholder = "Template (optional)",
           width = "95%"
         )
       )
     ),
     shiny::conditionalPanel(
       condition = "input.export_file != 'Place YAML on clipboard'",
       shiny::textInput(
         "file_path",
         "Path",
         value = "Untitled.Rmd",
         width = "95%"
       )
     ),
     height = 150
   ),
   scrollable = FALSE
 )
)
# R Markdown Options Panel ------------------------------------------------
rmarkdown_opts_panel <- miniUI::miniTabPanel(
 "R Markdown Options",
 miniUI::miniContentPanel(
   shiny::fillRow(
     shiny::textInput("keywords", "Keywords", width = "95%"),
     shiny::textInput("subject", "Subject", width = "95%"),
     shiny::textInput("category", "Categories", width = "95%"),
     height = 70
   ),
   shiny::fillRow(
     shiny::fillCol(
       shiny::textAreaInput("abstract", "Abstract", width = "400px", height = "60px"),
       shiny::textAreaInput("description", "Description", width = "400px", height = "60px")
     ),
     height = 200
   ),
   shiny::fillRow(
     shiny::selectizeInput(
       "runtime",
       "Runtime",
       choices = c("", "static", "shiny", "shiny_prerendered"),
       width = "95%"
     ),
     shiny::textInput("lang", "Language (IETF tag)", width = "95%"),
     height = 70
   ),
   shiny::fillRow(
     shiny::fileInput("resource_files", "Resource files", multiple = TRUE, width = "80%"),
     shiny::checkboxInput("clean", "Clean intermediate files", value = TRUE, width = "95%"),
     height = 70
   ),
   scrollable = FALSE
 ),
 icon = shiny::icon("data")
)
# LaTeX Options Panel -----------------------------------------------------
latex_opts_panel <- miniUI::miniTabPanel(
 "LaTeX Options",
 miniUI::miniContentPanel(
   shiny::fillRow(
     shiny::selectizeInput("fontsize", "Font Size", choices = c("", "10pt", "11pt", "12pt"), width = "95%"),
     shiny::textInput("fontfamily", "Font Family", width = "95%"),
     height = 70
   ),
   shiny::fillRow(
     shiny::textInput("linkcolor", "Internal Link Color", width = "95%"),
     shiny::textInput("citecolor", "Citation Link Color", width = "95%"),
     shiny::textInput("urlcolor", "External Link Color", width = "95%"),
     height = 70
   ),
   shiny::fillRow(
     shiny::fillCol(
       shiny::textInput("documentclass", "Document Class", width = "95%"),
       shiny::checkboxInput("indent", "Use Document Class Indentation")
     ),
     shiny::textInput("classoption", "Class Options", width = "95%"),
     height = 100
   ),
   shiny::fillRow(
     shiny::textInput("geometry", "Geometry", width = "95%"),
     height = 70
   ),
   shiny::fillRow(
     shiny::checkboxInput("links_as_notes", "Links as footnotes"),
     shiny::checkboxInput("lof", "Include List of Figures"),
     shiny::checkboxInput("lot", "Include List of Tables"),
     height = 45
   ),
   shiny::fillRow(
     shiny::textInput("thanks", "Thanks", width = "95%"),
     height = 70
   ),
   scrollable = FALSE
 ),
 icon = shiny::icon("data")
)
#  Citations Panel --------------------------------------------------------
citations_panel <- miniUI::miniTabPanel(
 "Citations",
 miniUI::miniContentPanel(
   shiny::fillRow(
     shiny::fileInput("bibliography", "Bibliography", width = "95%"),
     shiny::fileInput("csl", "CSL", width = "95%"),
     height = 70
   ),
   shiny::fillRow(
     shiny::checkboxInput("link_citations", "Add citations hyperlinks", width = "95%"),
     shiny::checkboxInput("suppress_bibliography", "Suppress bibliography", width = "95%"),
     height = 45
   ),
   shiny::h4("LaTeX Citations"),
   shiny::hr(),
   shiny::fillRow(
     shiny::textInput("biblio_title", "Bibliography Title", width = "95%"),
     height = 70
   ),
   shiny::fillRow(
     shiny::textInput("biblio_style", "Bibliography style", width = "95%"),
     height = 70
   ),
   shiny::fillRow(
     shiny::textInput("natbiboptions", "natbib options", width = "95%"),
     shiny::textInput("biblatexoptions", "biblatex options", width = "95%"),
     height = 70
   ),
   scrollable = FALSE
 ),
 icon = shiny::icon("data")
)
# Parameterized Reports Panel ---------------------------------------------
params_panel <- miniUI::miniTabPanel(
 "Parameterized Reports",
 miniUI::miniContentPanel(
   shiny::fillCol(
     flex = c(NA, 1, 0),
     shiny::fillRow(
       shiny::textInput("param", "Parameter", width = "95%"),
       shiny::textInput("param_value", "Value", width = "95%"),
       shiny::selectizeInput("shiny_fun", "Input", choices = shiny_functions, width = "95%"),
       shiny::textInput("param_label", "Label", width = "95%"),
       shiny::actionButton("add_param", "Add", style = "margin-top:25px"),
       height = 70,
       flex = c(rep(3, 4), 1)
     ),
     shiny::fillRow(
       shiny::tags$div(id = 'param_holder')
     ),
     shiny::fillRow(
       shiny::tags$div(id = 'modal_holder')
     )
   )
 ),
 icon = shiny::icon("data")
)

# UI ----------------------------------------------------------------------


shiny::shinyUI(miniUI::miniPage(
  miniUI::gadgetTitleBar(
    "Set up YAML",
    right = NULL
  ),
  miniUI::miniTabstripPanel(
    main_yaml_panel,
    rmarkdown_opts_panel,
    latex_opts_panel,
    citations_panel,
    params_panel
  ),
  miniUI::miniButtonBlock(
    miniUI::miniTitleBarButton("done", "Done", primary = TRUE)
  )
))
