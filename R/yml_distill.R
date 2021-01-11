#' Set Top-level YAML options for distill
#'
#' distill uses many custom YAML fields to create some of its unique features,
#' such as article metadata and citations. In addition to the arguments in
#' `yml_distill_opts()`, ymlthis supports distill in a number of other ways.
#' `yml_distill_author()` wraps [`yml_author()`] to include these extra used in
#' distill. For a distill blog, you can specify the listings page a post belongs
#' to, including an optional vector of other posts to list with it;
#' `distill_listing()` is a helper function to pass to the `listing` argument to
#' specify such pages. distill uses the same approach to navbars as R Markdown.
#' [`yml_navbar()`] and friends will help you write the YAML for that. YAML
#' specifying the site build, like the output field and navbars, can also be
#' placed in `_site.yml`; see [`yml_site_opts()`] for further R Markdown website
#' build options and [`use_site_yml()`] for creating that file based on a `yml`
#' object. distill's YAML options are discussed in greater detail in the
#' [articles on the distill website](https://rstudio.github.io/distill/).
#'
#' @template describe_yml_param
#' @param draft Logical. Set the post to be a draft? Draft posts won't be
#'   published.
#' @param slug The abbreviated version of the citation included in the BibTeX
#'   entry. If you don’t provide a slug then one will be automatically
#'   generated.
#' @param categories A character vector, the post categories
#' @param listing The listing a post is under; either a character vector, the
#'   output of `distill_listing()`, or a named list.
#' @param collection Specify the RSS, sharing, and other settings of a listing;
#'   use `distill_collection()` or a named list.
#' @param preview a path or link to the preview image for your article. You can
#'   also set this by including `preview = TRUE` in an R Markdown code chunk in
#'   your document.
#' @param repository_url A URL where the source code for your article can be
#'   found
#' @param citation_url A URL to the article; automatically generated for blog
#'   articles
#' @param compare_updates_url a URL that will show the differences between the
#'   article’s current version and the version that was initially published
#' @param base_url Base (root) URL for the location where the website will be
#'   deployed (used for providing preview images for Open Graph and Twitter
#'   Card)
#' @param creative_commons Designate articles that you create as Creative
#'   Commons licensed by specifying one of the standard Creative Commons
#'   licenses. Common options include "CC BY", "CC BY-SA", "CC BY-ND", and "CC
#'   BY-NC". See the [distill
#'   vignette](https://rstudio.github.io/distill/metadata.html) for more
#'   details.
#' @param twitter_site The Twitter handle for the site
#' @param twitter_creator The Twitter handle for the creator
#' @param journal_title The title of the journal
#' @param journal_issn The issn of the journal
#' @param journal_publisher The publisher of the journal
#' @param volume The volume the article is on
#' @param issue The issue the article is on
#' @param doi The article Digital Object Identifier (DOI)
#' @param resources Files to include or exclude while publishing. Use
#'   `distill_resources()` or a named list to specify.
#' @template describe_dots_param
#'
#' @template describe_yml_output
#' @export
#'
#' @examples
#' post_listing <- distill_listing(
#'   slugs = c(
#'     "2016-11-08-sharpe-ratio",
#'     "2017-11-09-visualizing-asset-returns",
#'     "2017-09-13-asset-volatility"
#'   )
#' )
#'
#' yml() %>%
#'   yml_title("Gallery of featured posts") %>%
#'   yml_distill_opts(listing = post_listing)
#'
#' yml_empty() %>%
#'   yml_title("Reproducible Finance with R") %>%
#'   yml_description("Exploring reproducible finance with the R statistical,
#'   computing environment.") %>%
#'   yml_site_opts(name = "reproducible-finance-with-r") %>%
#'   yml_distill_opts(
#'     base_url = "https://beta.rstudioconnect.com/content/3776/",
#'     collection = distill_collection(
#'       feed_items_max = 30,
#'       disqus_name = "reproducible-finance-with-r",
#'       disqus_hidden = FALSE,
#'       share = c("twitter", "linkedin")
#'     )
#'   )
#'
#' @family distill
#' @family websites
#' @seealso [`use_site_yml()`] [`use_rmarkdown()`]
yml_distill_opts <- function(
  .yml,
  draft = yml_blank(),
  slug = yml_blank(),
  categories = yml_blank(),
  listing = yml_blank(),
  collection = yml_blank(),
  citation_url = yml_blank(),
  preview = yml_blank(),
  repository_url = yml_blank(),
  base_url = yml_blank(),
  compare_updates_url = yml_blank(),
  creative_commons = yml_blank(),
  twitter_site = yml_blank(),
  twitter_creator = yml_blank(),
  journal_title = yml_blank(),
  journal_issn = yml_blank(),
  journal_publisher = yml_blank(),
  volume = yml_blank(),
  issue = yml_blank(),
  doi = yml_blank(),
  resources = yml_blank(),
  ...
) {

  twitter <- yml_blank()

  if (!is_yml_blank(journal_issn) || !is_yml_blank(journal_publisher)) {
    journal_title <- list(
      title = journal_title,
      issn = journal_issn,
      publisher = journal_title
    ) %>%
      purrr::discard(is_yml_blank)
  }

  if (!is_yml_blank(twitter_site) || !is_yml_blank(twitter_creator)) {
    twitter <- list(
      site = twitter_site,
      creator = twitter_creator
    ) %>% purrr::discard(is_yml_blank)
  }

  distill_opts <- list(
    draft = draft,
    slug = slug,
    categories = categories,
    listing = listing,
    collection = collection,
    preview = preview,
    repository_url = repository_url,
    citation_url = citation_url,
    base_url = base_url,
    compare_updates_url = compare_updates_url,
    creative_commons = creative_commons,
    twitter = twitter,
    journal = journal_title,
    volume = volume,
    issue = issue,
    doi = doi,
    resources = resources,
    ...
  ) %>%
    purrr::discard(is_yml_blank)

  warn_if_duplicate_fields(.yml, distill_opts)
  .yml[names(distill_opts)] <- distill_opts

  .yml
}

#' @inheritParams yml_author
#' @param url the author URL
#' @param affiliation_url the affiliation URL
#' @param orcid_id the author's ORCID ID
#'
#' @export
#'
#' @rdname yml_distill_opts
yml_distill_author <- function(
  .yml,
  name = yml_blank(),
  url = yml_blank(),
  affiliation = yml_blank(),
  affiliation_url = yml_blank(),
  orcid_id = yml_blank()
) {
  yml_author(
    .yml,
    name = name,
    url = url,
    affiliation = affiliation,
    affiliation_url = affiliation_url,
    orcid_id = orcid_id
  )
}

#' @param listing_name A character vector, the name of the listing
#' @param slugs A character vector of the posts to include in the listing
#'
#' @export
#' @rdname yml_distill_opts
distill_listing <- function(listing_name = "posts", slugs = NULL) {
  if (is.null(slugs)) {
    return(listing_name)
  }
  x <- list(slugs)
  names(x) <- listing_name

  x
}

#' @export
#'
#' @param collection_name A character vector, the name of the collection
#' @param feed_items_max Number of articles to include in the RSS feed (default:
#'   20). Specify `FALSE` to have no limit on the number of items included in
#'   the feed.
#' @param disqus_name A shortname for the disqus comments section (`base_url`
#'   field is required in order to use Disqus)
#' @param disqus_hidden Logical. Show full text of disqus comments? By default,
#'   this is `FALSE` so as not to obscure the bibliography and other appendices.
#' @param share Share buttons to include. Choices: "twitter", "linkedin",
#'   "facebook", "google-plus", and "pinterest". (`base_url` field is required
#'   in order to use sharing links)
#' @param citations Logical. If your `_site.yml` file provides a `base_url`
#'   field, an article citation appendix and related metadata will be included
#'   automatically within all published posts. Set to `FALSE` to disable this
#'   behavior.
#' @param subscribe a path to a HTML file enabling readers to subscribe. See the
#'   [distill vignette on blog
#'   posts](https://rstudio.github.io/distill/blog.html#creating-a-blog) for
#'   more details.
#'
#' @rdname yml_distill_opts
distill_collection <- function(
  collection_name = "post",
  feed_items_max = yml_blank(),
  disqus_name = yml_blank(),
  disqus_hidden = yml_blank(),
  share = yml_blank(),
  citations = yml_blank(),
  subscribe = yml_blank()
) {

  if (!is_yml_blank(disqus_hidden)) {
    disqus_name <- list(
      shortname = disqus_name,
      hiden = disqus_hidden
    )
  }

  x <- list(
    feed_items_max = feed_items_max,
    disqus = disqus_name,
    share = share,
    citations = citations,
    subscribe = subscribe
  ) %>%
    purrr::discard(is_yml_blank)

  if (purrr::is_empty(x)) {
    return(collection_name)
  }
  x <- list(x)
  names(x) <- collection_name

  x
}

#' @param include,exclude a character vector of files to explicitly include or
#'   exclude when publishing a post. Can use wild cards, such as "*.csv".
#'
#' @export
#' @rdname yml_distill_opts
distill_resources <- function(include = yml_blank(), exclude = yml_blank()) {
  list(include = include, exclude = exclude) %>%
    purrr::discard(is_yml_blank)
}
