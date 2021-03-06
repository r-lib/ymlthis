% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yml_blogdown.R
\name{yml_blogdown_opts}
\alias{yml_blogdown_opts}
\title{Set Top-level YAML options for blogdown}
\usage{
yml_blogdown_opts(
  .yml,
  draft = yml_blank(),
  publishdate = yml_blank(),
  weight = yml_blank(),
  slug = yml_blank(),
  aliases = yml_blank(),
  audio = yml_blank(),
  date = yml_blank(),
  description = yml_blank(),
  expiration_date = yml_blank(),
  headless = yml_blank(),
  images = yml_blank(),
  keywords = yml_blank(),
  layout = yml_blank(),
  lastmod = yml_blank(),
  link_title = yml_blank(),
  resources = yml_blank(),
  series = yml_blank(),
  summary = yml_blank(),
  title = yml_blank(),
  type = yml_blank(),
  url = yml_blank(),
  videos = yml_blank(),
  ...
)
}
\arguments{
\item{.yml}{a \code{yml} object created by \code{yml()}, \code{as_yml()}, or returned by
a \verb{yml_*()} function}

\item{draft}{Logical. Set post as a draft? Draft posts will not be rendered
if the site is built via \code{blogdown::build_site()} or
\code{blogdown::hugo_build()} but will be rendered in the local preview mode.
See \href{https://bookdown.org/yihui/blogdown/local-preview.html#local-preview}{Section D.3 of the blogdown book}.}

\item{publishdate}{A future date to publish the post. Future posts are only
rendered in the local preview mode}

\item{weight}{This field can take a numeric value to tell Hugo the order of
pages when sorting them, e.g., when you generate a list of all pages under
a directory, and two posts have the same date, you may assign different
weights to them to get your desired order on the list}

\item{slug}{A character string used as the tail of the post URL. It is
particularly useful when you define custom rules for permanent URLs. See
\href{https://bookdown.org/yihui/blogdown/configuration.html#options}{Section 2.2.2 of the blogdown book}.}

\item{aliases}{A character vector of one or more aliases (e.g., old published
paths of renamed content) that will be created in the output directory
structure}

\item{audio}{A character vector of paths to audio files related to the page}

\item{date}{The date assigned to this page. This is usually fetched from the
\code{date} field in front matter, but this behavior is configurable.}

\item{description}{The description for the content}

\item{expiration_date}{the date at which the content should no longer be
published by Hugo. Note that the actual YAML field is \code{expiryDate}}

\item{headless}{if \code{TRUE}, sets a leaf bundle to be
\href{https://gohugo.io/content-management/page-bundles/#headless-bundle}{headless}.}

\item{images}{A character vector of paths to images related to the page}

\item{keywords}{A character vector of the keywords for the content.}

\item{layout}{The layout Hugo should use while rendering the content. By
default, \code{layout} matches \code{type} and is thus based on the directory.
However, it's possible to use additional layouts within a type. See \href{https://gohugo.io/content-management/types/#defining-a-content-type}{Hugo's Defining a Content Type documentation}.}

\item{lastmod}{The date the content was last modified at}

\item{link_title}{used for creating links to content.  Note that the actual
YAML field is \code{linkTitle}}

\item{resources}{A named list. Used for configuring page bundle resources.
See \href{https://gohugo.io/content-management/page-resources/}{Hugo's Page Resources documentation}}

\item{series}{A character vector of series this page belongs to}

\item{summary}{A summary of the content in the \code{.Summary} Hugo page variable;
see the
\href{https://gohugo.io/content-management/summaries/}{content-summaries}
section of Hugo's documentation.}

\item{title}{The title for the content}

\item{type}{The type of the content, which is based on the from the directory
of the content if not specified}

\item{url}{The full path to the content from the web root}

\item{videos}{A character vector of paths to videos related to the page}

\item{...}{additional named R objects, such as characters or lists, to
transform into YAML}
}
\value{
a \code{yml} object
}
\description{
YAML in blogdown comes from a variety of sources. Most YAML will be for your
posts, as described in the \href{https://bookdown.org/yihui/blogdown/content.html#yaml-metadata}{blogdown book}).
Common R Markdown fields can be used, but there are two other main sources
for YAML fields: Hugo itself and the Hugo theme you are using. Hugo has
numerous top-level YAML to control the output (see the \href{https://gohugo.io/content-management/front-matter/}{Hugo documentation}).
\code{yml_blogdown_opts()} supports Hugo YAML. Your Hugo theme may also add fields
to use. To find YAML specific to your theme, see \code{\link[=blogdown_template]{blogdown_template()}}. In
addition to these sources of YAML, the configuration file for your blog can
also be in YAML, but this is not very common; most use a \code{config.toml} file,
based on TOML (see the \href{https://bookdown.org/yihui/blogdown/configuration.html}{blogdown book} for more).
}
\examples{

yml() \%>\%
  yml_blogdown_opts(
    draft = TRUE,
    slug = "blog-post"
  )
}
\seealso{
Other yml: 
\code{\link{asis_yaml_output}()},
\code{\link{bib2yml}()},
\code{\link{draw_yml_tree}()},
\code{\link{has_field}()},
\code{\link{read_json}()},
\code{\link{use_yml_defaults}()},
\code{\link{use_yml_file}()},
\code{\link{use_yml}()},
\code{\link{yml_author}()},
\code{\link{yml_bookdown_opts}()},
\code{\link{yml_citations}()},
\code{\link{yml_clean}()},
\code{\link{yml_distill_opts}()},
\code{\link{yml_latex_opts}()},
\code{\link{yml_output}()},
\code{\link{yml_pagedown_opts}()},
\code{\link{yml_params}()},
\code{\link{yml_pkgdown}()},
\code{\link{yml_reference}()},
\code{\link{yml_replace}()},
\code{\link{yml_resource_files}()},
\code{\link{yml_rsconnect_email}()},
\code{\link{yml_rticles_opts}()},
\code{\link{yml_runtime}()},
\code{\link{yml_site_opts}()},
\code{\link{yml_toc}()},
\code{\link{yml_vignette}()}
}
\concept{yml}
