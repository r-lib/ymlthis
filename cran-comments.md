## R CMD check results

0 errors | 0 warnings | 0 notes

## Release summary

This is a major version bump marking ymlthis as retired. The package will
remain on CRAN for existing users but will only receive critical maintenance
fixes going forward. Quarto now provides excellent YAML tooling that addresses
the needs ymlthis was created to solve.

This release also includes:
- Fixed deprecation warning from purrr 1.0.0: replaced `vec_depth()` with
  `pluck_depth()` (#91)
- Fixed NSE bug in `setup_chunk()` (#88)
- Updated all stale URLs (bookdown.org → yihui.org, docs.rstudio.com →
  docs.posit.co, gohugo.io content types)

## Reverse dependencies

We checked 3 reverse dependencies, comparing R CMD check results across CRAN
and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
