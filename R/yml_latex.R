#' Title
#'
#' @param block_headings
#' @param classoption
#' @param documentclass
#' @param geometry
#' @param indent
#' @param linestretch
#' @param margin_left
#' @param margin_right
#' @param margin_top
#' @param margin_bottom
#' @param pagestyle
#' @param papersize
#' @param secnumdepth
#' @param fontenc
#' @param fontfamily
#' @param fontfamilyoptions
#' @param fontsize
#' @param mainfont
#' @param sansfont
#' @param monofont
#' @param mathfont
#' @param CJKmainfont
#' @param mainfontoptions
#' @param sansfontoptions
#' @param monofontoptions
#' @param mathfontoptions
#' @param CJKoptions
#' @param microtypeoptions
#' @param colorlinks
#' @param linkcolor
#' @param filecolor
#' @param citecolor
#' @param urlcolor
#' @param toccolor
#' @param links_as_notes
#' @param lof
#' @param lot
#' @param thanks
#' @param toc
#' @param toc_depth
#' @param biblatexoptions
#' @param biblio_style
#' @param biblio_title
#' @param bibliography
#' @param natbiboptions
#'
#' @return
#' @export
#'
#' @examples
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

  .yml[names(latex_opts)] <- latex_opts

  .yml
}
