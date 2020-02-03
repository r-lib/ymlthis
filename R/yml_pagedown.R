#' Top-level YAML options for pagedown
#'
#' pagedown offers several output functions for paginated output, resumes,
#' business cards, theses, and morem as described in the [pagedown
#' vignette](https://pagedown.rbind.io/). pagedown also accepts a few custom
#' top-level YAML. See [pagedown_business_card_template()] for more on setting
#' up the YAML for a business card.
#'
#' @template describe_yml_param
#' @param toc Logical. Use a table of contents?
#' @param toc_title The title for the table of contents. Note that the actual
#'   YAML field is `toc-title`
#' @param lot Logical. Use a list of figures?
#' @param lot_title The title for the list of figures. Note that the actual YAML
#'   field is `lot-title`
#' @param chapter_name The chapter title prefix
#' @param links_to_footnotes Logical. Transform all the URLs to footnotes? Note
#'   that the actual YAML field is `links-to-footnotes`
#' @param paged_footnotes Logical. Render notes as footnotes? Note that the
#'   actual YAML field is `paged-footnotes`
#'
#' @template describe_yml_output
#' @export
#'
#' @examples
#'
#' yml() %>%
#'   yml_pagedown_opts(
#'     toc = TRUE,
#'     toc_title = "TOC",
#'     chapter_name = c("CHAPTER\\ ", "."),
#'     links_to_footnotes = TRUE
#'   )
#'
#' @family pagedown
yml_pagedown_opts <- function(
  .yml,
  toc = yml_blank(),
  toc_title = yml_blank(),
  lot = yml_blank(),
  lot_title = yml_blank(),
  chapter_name = yml_blank(),
  links_to_footnotes = yml_blank(),
  paged_footnotes = yml_blank()
) {
  pagedown_opts <- list(
    toc = toc,
    "toc-title" = toc_title,
    lot = lot,
    "lot-title" = lot_title,
    chapter_name = chapter_name,
    "links-to-footnotes" = links_to_footnotes,
    "paged-footnotes" = paged_footnotes
  ) %>%
    purrr::discard(is_yml_blank)

  warn_if_duplicate_fields(.yml, pagedown_opts)
  .yml[names(pagedown_opts)] <- pagedown_opts

  .yml
}

#' Generate a full YAML template for your pagedown business card
#'
#' pagedown has a unique output type to make business cards:
#' `pagedown::business_card()`. `pagedown_business_card_template()` creates a
#' YAML template to use for this output. What's unique about this output type is
#' that almost all of the contents are supplied through YAML. An R Markdown file
#' that only contains YAML related to the business card is enough to produce the
#' output, although you can also customize the output in the body of the
#' document (see the [pagedown vignette](https://pagedown.rbind.io/)). A good
#' workflow to write a business card is to use
#' `pagedown_business_card_template()` to specify the YAML and pass it to
#' [use_rmarkdown()], which you can then to knit into business cards.
#'
#' @param name The name
#' @param person When you are creating business cards for numerous people with
#'   shared information, passing values to the `person` field can override the
#'   default values, which can be any of the values accepted by this function.
#'   Use `pagedown_person()` to do so or manually provide them using `list(field
#'   = value)`.
#' @param title The title of the person
#' @param phone A phone number
#' @param email An email address
#' @param url A website URL
#' @param address The address
#' @param logo A path to a logo file
#' @param .repeat The number of cards to repeat. Note that the actual YAML field
#'   is `repeat`.
#' @param paperwidth The paper width
#' @param paperheight The paper height
#' @param cardwidth The width of the card
#' @param cardheight The height of the card
#' @param cols The number of columns in the card grid
#' @param rows The rows of columns in the card grid
#' @param mainfont The font
#' @param googlefonts A character vector of Google Fonts
#' @template describe_dots_param
#'
#' @template describe_yml_output
#' @export
#'
#' @examples
#' pagedown_business_card_template(
#'   name = "Jane Doe",
#'   title = "Miss Nobody",
#'   phone = "+1 123-456-7890",
#'   email = "jane.doe@example.com",
#'   url = "www.example.com",
#'   address = "2020 South Street,
#'   Sunshine, CA 90000",
#'   logo = "logo.png",
#'   .repeat = 12
#' )
#'
#' pagedown_business_card_template(
#'   phone = "+1 123-456-7890",
#'   url = "www.example.com",
#'   address = "2020 South Street,
#'   Sunshine, CA 90000",
#'   logo = "logo.png",
#'   person = list(
#'     pagedown_person(
#'       name = "Jane Doe",
#'       title = "Miss Nobody",
#'       email = "jane.doe@example.com",
#'       .repeat = 6
#'     ),
#'     pagedown_person(
#'       name = "John Doe",
#'       title = "Mister Nobody",
#'       phone = "+1 777-777-7777", # overrides the default phone
#'       email = "john.doe@example.com",
#'       .repeat = 6
#'     )
#'   ),
#'   paperwidth = "8.5in",
#'   paperheight = "11in",
#'   cols = 4,
#'   rows = 3
#' )
#'
#' @seealso [`use_rmarkdown()`]
#' @family pagedown
pagedown_business_card_template <- function(
  name = yml_blank(),
  person = yml_blank(),
  title = yml_blank(),
  phone = yml_blank(),
  email = yml_blank(),
  url = yml_blank(),
  address = yml_blank(),
  logo = yml_blank(),
  .repeat = yml_blank(),
  paperwidth = yml_blank(),
  paperheight = yml_blank(),
  cardwidth = yml_blank(),
  cardheight = yml_blank(),
  cols = yml_blank(),
  rows = yml_blank(),
  mainfont = yml_blank(),
  googlefonts = yml_blank(),
  ...
) {
  list(
    name = name,
    person = person,
    title = title,
    phone = phone,
    email = email,
    url = url,
    address = address,
    logo = logo,
    "repeat" = .repeat,
    paperwidth = paperwidth,
    paperheight = paperheight,
    cardwidth = cardwidth,
    cardheight = cardheight,
    cols = cols,
    rows = rows,
    mainfont = mainfont,
    googlefonts = googlefonts,
    output = "pagedown::business_card",
    ...
  ) %>%
    purrr::discard(is_yml_blank) %>%
    as_yml()
}

#' @export
#' @rdname pagedown_business_card_template
pagedown_person <- function(...) {
  pagedown_business_card_template(...) %>%
    yml_discard("output") %>%
    unclass()
}
