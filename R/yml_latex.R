#' Set LaTeX YAML options for PDF output
#'
#' `yml_latex_opts()` sets top-level YAML fields for LaTeX options used by
#' pandoc ([see the documentation](https://pandoc.org/MANUAL.html)), as when
#' making a PDF document with `pdf_document()`.
#'
#' @template describe_yml_param
#' @param block_headings make paragraph and subparagraph (fourth- and
#'   fifth-level headings, or fifth- and sixth-level with book classes)
#'   free-standing rather than run-in; requires further formatting to distinguish
#'   from subsubsection (third- or fourth-level headings)
#' @param classoption option for document class, e.g. oneside; repeat for
#'   multiple options.
#' @param documentclass document class: usually one of the standard classes,
#'   article, book, and report
#' @param geometry option for geometry package, e.g. margin=1in; repeat for
#'   multiple options:
#' @param indent uses document class settings for indentation (the default LaTeX
#'   template otherwise removes indentation and adds space between paragraphs)
#' @param linestretch adjusts line spacing using the setspace package, e.g. 1.25,
#'   1.5
#' @param margin_left,margin_right,margin_top,margin_bottom sets margins if
#'   geometry is not used (otherwise geometry overrides these)
#' @param pagestyle control pagestyle{}: the default article class supports
#'   plain (default), empty (no running heads or page numbers), and headings
#'   (section titles in running heads)
#' @param papersize paper size, e.g. letter, a4
#' @param secnumdepth numbering depth for sections (with --number-sections option
#'   or numbersections variable)
#' @param fontenc allows font encoding to be specified through fontenc package
#'   (with pdflatex); default is T1 (see [LaTeX font encodings
#'   guide](https://ctan.org/pkg/encguide))
#' @param fontfamily font package for use with pdflatex: TeX Live includes many
#'   options, documented in the LaTeX Font Catalogue. The default is Latin
#'   Modern.
#' @param fontfamilyoptions options for package used as fontfamily; repeat for
#'   multiple options.
#' @param fontsize font size for body text. The standard classes allow 10pt,
#'   11pt, and 12pt.
#' @param mainfont,sansfont,monofont,mathfont,CJKmainfont font families for use
#'   with xelatex or lualatex: take the name of any system font, using the
#'   fontspec package. CJKmainfont uses the xecjk package.
#' @param
#' mainfontoptions,sansfontoptions,monofontoptions,mathfontoptions,CJKoptions
#' options to use with mainfont, sansfont, monofont, mathfont, CJKmainfont in
#' xelatex and lualatex. Allow for any choices available through fontspec; repeat
#' for multiple options.
#' @param microtypeoptions options to pass to the microtype LaTeX package
#' @param colorlinks add color to link text; automatically enabled if any of
#'   linkcolor, filecolor, citecolor, urlcolor, or toccolor are set
#' @param linkcolor,filecolor,citecolor,urlcolor,toccolor color for internal
#'   links, external links, citation links, linked URLs, and links in table of
#'   contents, respectively: uses options allowed by xcolor, including the
#'   dvipsnames, svgnames, and x11names lists
#' @param links_as_notes causes links to be printed as footnotes
#' @param lof,lot include list of figures, list of tables
#' @param thanks contents of acknowledgments footnote after document title
#' @param toc include table of contents
#' @param toc_depth level of section to include in table of contents
#' @param biblatexoptions list of options for biblatex
#' @param biblio_style bibliography style, when used with natbib and biblatex.
#' @param biblio_title bibliography title, when used with natbib and biblatex.
#' @param bibliography bibliography to use for resolving references
#' @param natbiboptions list of options for natbib
#'
#' @return a `yml` object
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
