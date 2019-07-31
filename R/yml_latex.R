#' Set LaTeX YAML options for PDF output
#'
#' `yml_latex_opts()` sets top-level YAML fields for LaTeX options used by
#' pandoc ([see the documentation](https://pandoc.org/MANUAL.html), from which
#' these descriptions were derived), as when making a PDF document with
#' `pdf_document()`.
#'
#' @template describe_yml_param
#' @param block_headings make paragraph and subparagraph (fourth- and
#'   fifth-level headings, or fifth- and sixth-level with book classes)
#'   free-standing rather than run-in; requires further formatting to
#'   distinguish from subsubsection (third- or fourth-level headings). Note that
#'   the YAML field is actually called `block-headings`.
#' @param classoption a character vector of options for document class, e.g.
#'   "oneside"
#' @param documentclass the document class usually "article", "book", or
#'   "report"
#' @param geometry a character vector of options for the [geometry LaTeX
#'   package](https://ctan.org/pkg/geometry?lang=en), e.g. "margin=1in"
#' @param indent Logical. Use document class settings for indentation? The
#'   default LaTeX template otherwise removes indentation and adds space between
#'   paragraphs.
#' @param linestretch adjusts line spacing using the [setspace LaTeX
#'   package](https://ctan.org/pkg/setspace?lang=en), e.g. 1.25, 1.5
#' @param margin_left,margin_right,margin_top,margin_bottom sets margins if
#'   `geometry` is not used, otherwise `geometry` overrides these. Note that the
#'   actual YAML fields use `-` instead of `_`, e.g. `margin-left`.
#' @param pagestyle control the `pagestyle` LaTeX command: the default article
#'   class supports "plain" (default), "empty" (no running heads or page
#'   numbers), and "headings" (section titles in running heads)
#' @param papersize paper size, e.g. letter, a4
#' @param secnumdepth numbering depth for sections (with `--number-sections`
#'   pandoc)
#' @param fontenc allows font encoding to be specified through [fontenc LaTeX
#'   package](https://www.ctan.org/pkg/fontenc) (with pdflatex); default is "T1"
#'   (see [LaTeX font encodings guide](https://ctan.org/pkg/encguide))
#' @param fontfamily font package for use with pdflatex: TeX Live includes many
#'   options, documented in the [LaTeX Font
#'   Catalogue](https://tug.dk/FontCatalogue/). The default is "Latin Modern".
#' @param fontfamilyoptions a character vector of options for `fontfamily`.
#' @param fontsize font size for body text. The standard classes allow "10pt",
#'   "11pt", and "12pt".
#' @param mainfont,sansfont,monofont,mathfont,CJKmainfont font families for use
#'   with xelatex or lualatex: take the name of any system font, using the
#'   [fontspec LaTeX package](https://www.ctan.org/pkg/fontspec). CJKmainfont
#'   uses the [xecjk LaTeX package.](https://www.ctan.org/pkg/xecjk).
#' @param
#' mainfontoptions,sansfontoptions,monofontoptions,mathfontoptions,CJKoptions a
#' character vector of options to use with mainfont, sansfont, monofont,
#' mathfont, CJKmainfont in xelatex and lualatex. Allow for any choices
#' available through fontspec.
#' @param microtypeoptions a character vector of options to pass to the
#'   [microtype LaTeX package](https://www.ctan.org/pkg/microtype).
#' @param colorlinks Logical. Add color to link text? Automatically enabled if
#'   any of `linkcolor`, `filecolor`, `citecolor`, `urlcolor`, or `toccolor` are
#'   set.
#' @param linkcolor,filecolor,citecolor,urlcolor,toccolor color for internal
#'   links, external links, citation links, linked URLs, and links in table of
#'   contents, respectively: uses options allowed by
#'   [xcolor](https://ctan.org/pkg/xcolor?lang=en), including the dvipsnames,
#'   svgnames, and x11names lists
#' @param links_as_notes Logical. Print links as footnotes? Note that the actual
#'   YAML field is `links-as-notes`
#' @param lof,lot Logical. Include list of figures or list of tables?
#' @param thanks contents of acknowledgments footnote after document title
#' @param toc include table of contents
#' @param toc_depth level of section to include in table of contents. Note that
#'   the actual YAML field is `toc-depth`
#' @param biblatexoptions list of options for
#'   [biblatex](https://ctan.org/pkg/biblatex).
#' @param biblio_style bibliography style, when used with
#'   [natbib](https://ctan.org/pkg/natbib) and
#'   [biblatex](https://ctan.org/pkg/biblatex). Note that the actual YAML field
#'   is `biblio-style`
#' @param biblio_title bibliography title, when used with
#'   [natbib](https://ctan.org/pkg/natbib) and
#'   [biblatex](https://ctan.org/pkg/biblatex). Note that the actual YAML field
#'   is `biblio-title`
#' @param bibliography a path to the bibliography file to use for references
#' @param natbiboptions a character vector of options for
#'   [natbib](https://ctan.org/pkg/natbib)
#'
#' @template describe_yml_output
#' @export
#'
#' @examples
#'
#' yml() %>%
#'    yml_output(pdf_document()) %>%
#'    yml_latex_opts(
#'      fontfamily = "Fira Sans Thin",
#'      fontsize = "11pt",
#'      links_as_note = TRUE
#'    )
#'
yml_latex_opts <- function(
  .yml,
  block_headings = yml_blank(),
  classoption = yml_blank(),
  documentclass = yml_blank(),
  geometry = yml_blank(),
  indent = yml_blank(),
  linestretch = yml_blank(),
  margin_left = yml_blank(),
  margin_right = yml_blank(),
  margin_top = yml_blank(),
  margin_bottom = yml_blank(),
  pagestyle = yml_blank(),
  papersize = yml_blank(),
  secnumdepth = yml_blank(),
  fontenc = yml_blank(),
  fontfamily = yml_blank(),
  fontfamilyoptions = yml_blank(),
  fontsize = yml_blank(),
  mainfont = yml_blank(),
  sansfont = yml_blank(),
  monofont = yml_blank(),
  mathfont = yml_blank(),
  CJKmainfont = yml_blank(),
  mainfontoptions = yml_blank(),
  sansfontoptions = yml_blank(),
  monofontoptions = yml_blank(),
  mathfontoptions = yml_blank(),
  CJKoptions = yml_blank(),
  microtypeoptions = yml_blank(),
  colorlinks = yml_blank(),
  linkcolor = yml_blank(),
  filecolor = yml_blank(),
  citecolor = yml_blank(),
  urlcolor = yml_blank(),
  toccolor = yml_blank(),
  links_as_notes = yml_blank(),
  lof = yml_blank(),
  lot = yml_blank(),
  thanks = yml_blank(),
  toc = yml_blank(),
  toc_depth = yml_blank(),
  biblatexoptions = yml_blank(),
  biblio_style = yml_blank(),
  biblio_title = yml_blank(),
  bibliography = yml_blank(),
  natbiboptions = yml_blank()
) {
  latex_opts <- list(
    "block-headings" = block_headings,
    classoption = classoption,
    documentclass = documentclass,
    geometry = geometry,
    indent = indent,
    linestretch = linestretch,
    "margin-left" = margin_left,
    "margin-right" = margin_right,
    "margin-top" = margin_top,
    "margin-bottom" = margin_bottom,
    pagestyle = pagestyle,
    papersize = papersize,
    secnumdepth = secnumdepth,
    fontenc = fontenc,
    fontfamily = fontfamily,
    fontfamilyoptions = fontfamilyoptions,
    fontsize = fontsize,
    mainfont = mainfont,
    sansfont = sansfont,
    monofont = monofont,
    mathfont = mathfont,
    CJKmainfont = CJKmainfont,
    mainfontoptions = mainfontoptions,
    sansfontoptions = sansfontoptions,
    monofontoptions = monofontoptions,
    mathfontoptions = mathfontoptions,
    CJKoptions = CJKoptions,
    microtypeoptions = microtypeoptions,
    colorlinks = colorlinks,
    linkcolor = linkcolor,
    filecolor = filecolor,
    citecolor = citecolor,
    urlcolor = urlcolor,
    toccolor = toccolor,
    "links-as-notes" = links_as_notes,
    lof = lof,
    lot = lot,
    thanks = thanks,
    toc = toc,
    "toc-depth" = toc_depth,
    biblatexoptions = biblatexoptions,
    "biblio-style" = biblio_style,
    "biblio-title" = biblio_title,
    bibliography = bibliography,
    natbiboptions = natbiboptions
  )

  latex_opts <- purrr::discard(latex_opts, is_yml_blank)

  warn_if_duplicate_fields(.yml, latex_opts)
  .yml[names(latex_opts)] <- latex_opts

  .yml
}
