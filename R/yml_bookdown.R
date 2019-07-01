#' Top-level YAML options for bookdown
#'
#' * mention `use_bookdown_yml()` and `use_output_yml()` re = https =//bookdown.org/yihui/rmarkdown/bookdown-project.html
#'
#' @param book_filename a character vector, the filename of the main Rmd file,
#'   i.e., the Rmd file that is merged from all chapters. By default, it is
#'   named "_main.Rmd".
#' @param delete_merged_file logical. Delete the main Rmd file if it exists?
#' @param before_chapter_script one or multiple R scripts to be executed before
#'   each chapter
#' @param after_chapter_script similar to `before_chapter_script`, but the R
#'   script is executed after each chapter
#' @param edit a URL that collaborators can click to edit the Rmd source
#'   document of the current page; this was designed primarily for GitHub
#'   repositories, since it is easy to edit arbitrary plain-text files on GitHub
#'   even in other people’s repositories (if you do not have write access to the
#'   repository, GitHub will automatically fork it and let you submit a pull
#'   request after you finish editing the file). This link should have `%s` in
#'   it, which will be substituted by the actual Rmd filename for each page.
#' @param history similar to `edit`, a link to the edit/commit history of the
#'   current page
#' @param rmd_files a character vector, the order order of Rmd files for the
#'   book. `rmd_files` can also be a named list where each element of the list
#'   is named for the output type, e.g. "html" or "latex". By default, bookdown
#'   merges all Rmd files by the order of filenames.
#' @param rmd_subdir whether to search for book source Rmd files in
#'   subdirectories (by default, only the root directory is searched). This may
#'   be either a boolean (e.g. true will search for book source Rmd files in the
#'   project directory and all subdirectories) or list of paths if you want to
#'   search for book source Rmd files in a subset of subdirectories
#' @param output_dir the output directory of the book ("_book" by default); this
#'   setting is read and used by
#' @param clean a character vector of files and directories to be cleaned by the
#'   `bookdown::clean_book()` function.
#'
#' @return
#' @export
#'
#' @examples
#'
#' yml(author = FALSE, date = FALSE) %>%
#'   yml_bookdown_opts(
#'     book_filename = "my-book.Rmd",
#'     before_chapter_script = c("script1.R", "script2.R"),
#'     after_chapter_script = "script3.R",
#'     edit = "https =//github.com/rstudio/bookdown-demo/edit/master/%s",
#'     output_dir = "book-output",
#'     clean = c("my-book.bbl", "R-packages.bib")
#'  )
#'
#' yml(author = FALSE, date = FALSE) %>%
#'   yml_bookdown_opts(
#'     rmd_files = list(
#'       html = c("index.Rmd", "abstract.Rmd", "intro.Rmd"),
#'       latex = c("abstract.Rmd", "intro.Rmd")
#'     )
#'  )
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
  clean = yml_blank()
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
    clean = clean
  ) %>%
    purrr::discard(is_yml_blank)

  warn_if_duplicate_fields(.yml, bookdown_opts)
  .yml[names(bookdown_opts)] <- bookdown_opts

  .yml
}


#' Title
#'
#' * defaults and descriptions are available at https://bookdown.org/yihui/bookdown/html.html
#'
#' @param toc_collapse
#' @param toc_scroll_highlight
#' @param toc_before
#' @param toc_after
#' @param toolbar_position
#' @param edit
#' @param download
#' @param search
#' @param fontsettings_theme
#' @param fontsettings_family
#' @param fontsettings_size
#' @param sharing_facebook
#' @param sharing_twitter
#' @param sharing_google
#' @param sharing_linkedin
#' @param sharing_weibo
#' @param sharing_instapaper
#' @param sharing_vk
#' @param sharing_all
#'
#' @return
#' @export
#'
#' @examples
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
  sharing_all = yml_blank()
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
      purrr::discard(is_yml_blank)
  ) %>%
    purrr::discard(is_yml_blank) %>%
    purrr::discard(purrr::is_empty)
}
