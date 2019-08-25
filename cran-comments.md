## Test environments
* local OS X install, R 3.5.3
* ubuntu 14.04 (on travis-ci), R 3.5.3
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
* This is a re-submission. The following errors were fixed:
  - Fixed test error on certain OSs where object contents were out of order
  - Changed link to shinyapps to a static site to avoid curl connection problems
