#' Set Top-level YAML options for bookdown
#'
#' bookdown uses YAML in three main places, as described in the [bookdown
#' book](https://bookdown.org/yihui/rmarkdown/bookdown-project.html):
#' `index.Rmd`, `_output.yml`, and `_bookdown.yml`. `index.Rmd` can take most
#' YAML. `_output.yml` is intended for output-related YAML, such as that
#' produced by `yml() %>% yml_output(bookdown::pdf_book())`. `_bookdown.yml` is
#' intended for configuring the build of the book. Pass the results of the
#' `yml_*()` functions to `use_index_rmd()`, `use_bookdown_yml()`,
#' `use_output_yml()` to write them to these files. `yml_bookdown_site()` adds
#' the `site: "bookdown::bookdown_site"` to the YAML metadata.
#'
#' @template describe_yml_param
#' @param book_filename A character vector, the filename of the main `.Rmd`
#'   file, the `.Rmd` file that is created by merging all chapters. By default,
#'   it is called "_main.Rmd".
#' @param delete_merged_file Logical. Delete the main `.Rmd` file if it exists?
#' @param before_chapter_script,after_chapter_script A character vector of one
#'   or more R scripts to be executed before or after each chapter
#' @param edit A URL that collaborators can click to edit the `.Rmd` source
#'   document of the current page, usually a link to a GitHub repository. This
#'   link should have `%s` where the actual `.Rmd` filename for each page will
#'   go.
#' @param history Similar to `edit`, a link to the edit/commit history of the
#'   current page.
#' @param rmd_files A character vector, the order order of `.Rmd` files for the
#'   book. `rmd_files` can also be a named list where each element of the list
#'   is named for the output type, e.g. "html" or "latex". By default, bookdown
#'   merges all `.Rmd` files by the order of filenames.
#' @param rmd_subdir whether to search for book source `.Rmd` files in
#'   subdirectories (by default, only the root directory is searched). This may
#'   be either a boolean (e.g. `TRUE` will search for book source `.Rmd` files
#'   in the project directory and all subdirectories) or vector of paths if you
#'   want to search for book source `.Rmd` files in a subset of subdirectories.
#' @param output_dir the output directory of the book ("_book" by default)
#' @param clean a character vector of files and directories to be cleaned by the
#'   `bookdown::clean_book()` function.
#' @template describe_dots_param
#'
#' @template describe_yml_output
#' @export
#'
#' @examples
#'
#' yml_empty() %>%
#'   yml_bookdown_opts(
#'     book_filename = "my-book.Rmd",
#'     before_chapter_script = c("script1.R", "script2.R"),
#'     after_chapter_script = "script3.R",
#'     edit = "https =//github.com/rstudio/bookdown-demo/edit/master/%s",
#'     output_dir = "book-output",
#'     clean = c("my-book.bbl", "R-packages.bib")
#'  )
#'
#' yml_empty() %>%
#'   yml_bookdown_opts(
#'     rmd_files = list(
#'       html = c("index.Rmd", "abstract.Rmd", "intro.Rmd"),
#'       latex = c("abstract.Rmd", "intro.Rmd")
#'     )
#'  )
#'
#'  x <- yml_empty() %>%
#'   yml_title("A Minimal Book Example") %>%
#'   yml_date(yml_code(Sys.Date())) %>%
#'   yml_author("Yihui Xie") %>%
#'   yml_bookdown_site() %>%
#'   yml_latex_opts(
#'     documentclass = "book",
#'     bibliography = c("book.bib", "packages.bib"),
#'     biblio_style = "apalike"
#'   ) %>%
#'   yml_citations(
#'     link_citations = TRUE
#'   ) %>%
#'   yml_description("This is a minimal example of using
#'   the bookdown package to write a book.")
#'
#' x
#'
#' # alternatively use `use_index_rmd()` to write to file
#' # use_index_rmd(x)
#'
#' output_yml <- yml_empty() %>%
#'   yml_output(
#'     bookdown::gitbook(
#'       lib_dir = "assets",
#'       split_by = "section",
#'       config = gitbook_config(toolbar_position = "static")
#'     ),
#'     bookdown::pdf_book(keep_tex = TRUE),
#'     bookdown::html_book(css = "toc.css")
#'   )
#'
#' output_yml
#'
#' # alternatively use `use_output_yml()` to write to `_output.yml`
#' # use_output_yml(output_yml)
#'
#' @family bookdown
#' @seealso [`use_index_rmd()`] [`use_bookdown_yml()`] [`use_output_yml()`]
yml_bookdown_opts <- function(
  .yml,
  book_filename = yml_blank(),
  delete_merged_file = yml_blank(),
  before_chapter_script = yml_blank(),
  after_chapter_script = yml_blank(),
  edit = yml_blank(),
  history = yml_blank(),
  rmd_files = yml_blank(),
  rmd_subdir = yml_blank(),
  output_dir = yml_blank(),
  clean = yml_blank(),
  ...
) {
  bookdown_opts <- list(
    book_filename = book_filename,
    delete_merged_file = delete_merged_file,
    before_chapter_script = before_chapter_script,
    after_chapter_script = after_chapter_script,
    edit = edit,
    history = history,
    rmd_files = rmd_files,
    rmd_subdir = rmd_subdir,
    output_dir = output_dir,
    clean = clean,
    ...
  ) %>%
    purrr::discard(is_yml_blank)

  warn_if_duplicate_fields(.yml, bookdown_opts)
  .yml[names(bookdown_opts)] <- bookdown_opts

  .yml
}


#' @export
#' @rdname yml_bookdown_opts
yml_bookdown_site <- function(.yml) {
  warn_if_duplicate_fields(.yml, list(site = ""))
  .yml$site <- "bookdown::bookdown_site"
  .yml
}


#' Configure `bookdown::gitbook()` output
#'
#' `gitbook_config()` is a helper function to specify the `config` argument in
#' [bookdown::gitbook()], as described in the [bookdown
#' book](https://bookdown.org/yihui/bookdown/html.html).
#'
#' @param toc_collapse Collapse some items initially when a page is loaded via
#'   the collapse option. Its possible values are "subsection" (the default),
#'   "section", "none", or `NULL`.
#' @param toc_scroll_highlight Logical. Enable highlighting of TOC items as you
#'   scroll the book body? The default is `TRUE`.
#' @param toc_before,toc_after a character vector of HTML to add more items
#'   before and after the TOC using the HTML tag `<li>`. These items will be
#'   separated from the TOC using a horizontal divider.
#' @param toolbar_position The toolbar position: "fixed" or "static." The
#'   default ("fixed") is that the toolbar will be fixed at the top of the page,
#'   whereas when set to "static" the toolbar will not scroll with the page.
#' @param edit If not empty, an edit button will be added to the toolbar.
#' @param download This option takes either a character vector or a list of
#'   character vectors with the length of each vector being 2. When it is a
#'   character vector, it should be either a vector of filenames or filename
#'   extensions. When you only provide the filename extensions, the filename is
#'   derived from the book filename of the configuration file `_bookdown.yml`
#' @param search Include a search bar?
#' @param fontsettings_theme The theme. "White" (the default), "Sepia", or
#'   "Night".
#' @param fontsettings_family The font family. "sans" (the default) or "serif".
#' @param fontsettings_size The font size. Default is 2.
#' @param sharing_facebook Logical. Include Facebook share link? Default is
#'   `TRUE`.
#' @param sharing_twitter Logical. Include Twitter share link? Default is
#'   `TRUE`.
#' @param sharing_google Logical. Include Google share link? Default is `FALSE`.
#' @param sharing_linkedin Logical. Include LinkedIn share link? Default is
#'   `FALSE`.
#' @param sharing_weibo Logical. Include Weibo share link? Default is `FALSE`.
#' @param sharing_instapaper Logical. Include Instapaper share link? Default is
#'   `FALSE`.
#' @param sharing_vk Logical. Include VK share link? Default is `FALSE`.
#' @param sharing_all Logical. Include all share links? Default is `FALSE`.
#' @template describe_dots_param
#'
#' @return a list to use in the `config` argument of [bookdown::gitbook()]
#' @export
#'
#' @family bookdown
gitbook_config <- function(
  toc_collapse = yml_blank(),
  toc_scroll_highlight = yml_blank(),
  toc_before = yml_blank(),
  toc_after = yml_blank(),
  toolbar_position = yml_blank(),
  edit = yml_blank(),
  download = yml_blank(),
  search = yml_blank(),
  fontsettings_theme = yml_blank(),
  fontsettings_family = yml_blank(),
  fontsettings_size = yml_blank(),
  sharing_facebook = yml_blank(),
  sharing_twitter = yml_blank(),
  sharing_google = yml_blank(),
  sharing_linkedin = yml_blank(),
  sharing_weibo = yml_blank(),
  sharing_instapaper = yml_blank(),
  sharing_vk = yml_blank(),
  sharing_all = yml_blank(),
  ...
) {
  list(
    toc = list(
      collapse = toc_collapse,
      scroll_highlight = toc_scroll_highlight,
      before = toc_before,
      after = toc_after
    ) %>%
      purrr::discard(is_yml_blank),
    toolbar = list(position = toolbar_position) %>%
      purrr::discard(is_yml_blank),
    edit = edit,
    download = download,
    search = search,
    fontsettings = list(
      theme = fontsettings_theme,
      family = fontsettings_family,
      size = fontsettings_size
    ) %>%
      purrr::discard(is_yml_blank),
    sharing = list(
      facebook = sharing_facebook,
      twitter = sharing_twitter,
      google = sharing_google,
      linkedin = sharing_linkedin,
      weibo = sharing_weibo,
      instapaper = sharing_instapaper,
      vk = sharing_vk,
      all = sharing_all
    ) %>%
      purrr::discard(is_yml_blank),
    ...
  ) %>%
    purrr::discard(is_yml_blank) %>%
    purrr::discard(purrr::is_empty)
}
