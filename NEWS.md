# ymlthis 2.0.0

# ymlthis 1.0.0

## Lifecycle

* ymlthis is now retired. Only changes necessary to keep it on CRAN will be
  made. Quarto now provides excellent YAML support, making a specialized
  YAML-writing package less necessary.

## Bug fixes

* Fixed deprecation warning from purrr: replaced `vec_depth()` with
  `pluck_depth()` (#91).
* Fixed bug in `setup_chunk()` where `chunk_code` expressions were not
  properly captured (#88, thanks @dchiu911).

# ymlthis 0.1.7
* Fix typo in add-in checking for miniUI (#84, #85, thanks @tonycmac &
  @lquayle88)

# ymlthis 0.1.6
* `yml_author()` now correctly accepts `yml_blank()` as needed (#71)
* Fix issues with new shiny release (#80)

# ymlthis 0.1.5
* Update citeproc functionality to rely on newer rmarkdown functions

# ymlthis 0.1.3
* Minor changes to address upcoming changes in shiny 1.6

# ymlthis 0.1.2
 
* updated roxygen2 rendering and removed unnecessary `...` description from `yml_pagedown_opts()`
* fixed bug where removing a blank line did not work because it called the wrong object (issue #52, thanks @dchiu911) 

# ymlthis 0.1.1

* Fixed errors in the fieldguide introduced by changes in roxygen2 7.0.0 (#50)
* Add option `ymlthis.remove_blank_line`. If `TRUE`, YAML files will have their final lines removed (#42)
* Synced arguments for `use_rmarkdown()` and `use_index_rmd()` and added `open_doc` argument to disable opening file (#41)
* Fix bug where `usethis::write_over()` was not getting the `quiet` argument set correctly (issue #37)
* Added `biblio_style` and `biblio_title` to `yml_citations()` (issue #40)

# ymlthis 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* Initial release of package
