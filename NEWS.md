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
