## Test environments
* local OS X install, R 3.5.3
* ubuntu 14.04 (on travis-ci), R 3.5.3
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
* This is a third re-submission. The following problems were addressed:
  - Quotation usage in DESCRIPTION file fixed
  - Changed dontrun{} examples to donttest{}
* This is a second re-submission. The following problems were addressed:
  - Confirmed author list; all mentions of "author" in package documentation are strictly related to examples for the `author` YAML field and not related to development of this package.
  - Added reset of `option()` in vignettes
  - Removed commented code in examples
  - Confirmed necessity of `/dontrun{}` examples: all cases require additional software, e.g. pandoc. However, I added tests to address most of these that check for the existence of the required software to improve example robustness.
  - Confirmed use of `cat()` (instead of e.g. `message()`) only being used in cases where printing directly to the console is relevant; all such cases use either a `print()` method or via the usethis package.
  - Confirmed that all examples and vignettes write to `tempfile()` and not `getwd()`
* In the first re-submission, the following errors were fixed:
  - Fixed test error on certain OSs where object contents were out of order
  - Changed link to shinyapps to a static site to avoid curl connection problems
