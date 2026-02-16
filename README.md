
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ymlthis: write YAML for R Markdown, bookdown, blogdown, and more

<!-- badges: start -->

[![R-CMD-check](https://github.com/r-lib/ymlthis/workflows/R-CMD-check/badge.svg)](https://github.com/r-lib/ymlthis/actions)
[![Codecov test
coverage](https://codecov.io/gh/r-lib/ymlthis/branch/main/graph/badge.svg)](https://app.codecov.io/gh/r-lib/ymlthis?branch=main)
[![Lifecycle:
retired](https://img.shields.io/badge/lifecycle-retired-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#retired)
[![CRAN
status](https://www.r-pkg.org/badges/version/ymlthis)](https://cran.r-project.org/package=ymlthis)
<!-- badges: end -->

## Status: Retired

ymlthis is now retired: only changes necessary to keep it on CRAN will
be made. ymlthis was created when managing YAML in R Markdown was
difficult. [Quarto](https://quarto.org/) has since resolved these issues
with autocomplete, validation, and helpful error messages for YAML. We
recommend Quarto for new projects. Existing ymlthis code will continue
to work, and the package will remain on CRAN.

ymlthis makes it easy to write YAML front matter for R Markdown and
related documents. `yml_*()` functions write functions and `use_*()`
functions let you write the resulting YAML to your clipboard or to
`.yml` files related to your project.

## Installation

You can install ymlthis from CRAN with:

``` r
install.packages("ymlthis")
```

Or you can install the development version of ymlthis from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("r-lib/ymlthis")
```

## Example

`yml()` creates a basic `yml` object returns simple YAML with the author
and date.

``` r
library(ymlthis)

yml()
#> ---
#> author: Malcolm Barrett
#> date: '`r format(Sys.Date())`'
#> ---
```

ymlthis supports many YAML arguments, with YAML-generating functions
prefixed with `yml_*()`:

``` r
yml() %>% 
  yml_author(c("Yihui Xie", "Hadley Wickham"), affiliation = "RStudio") %>% 
  yml_date(lubridate::today()) %>% 
  yml_output(
    word_document(keep_md = TRUE), 
    bookdown::html_document2()
  ) %>% 
  yml_citations(bibliography = "references.bib", csl = "aje.csl")
#> ---
#> author:
#> - name: Yihui Xie
#>   affiliation: RStudio
#> - name: Hadley Wickham
#>   affiliation: RStudio
#> date: '2026-02-16'
#> output:
#>   word_document:
#>     keep_md: true
#>   bookdown::html_document2: default
#> bibliography: references.bib
#> csl: aje.csl
#> ---
```

## Add-in

ymlthis also includes an add-in that will create YAML for you and put it
in a file, such as an `.Rmd` file, or on your clipboard.

![](https://i.imgur.com/BkzGueG.gif)
