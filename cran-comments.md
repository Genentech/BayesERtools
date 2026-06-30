## Submission notes

This release fixes the test failure reported on CRAN's r-devel checks, which
was caused by a change in `loo` 2.10.0: `loo_compare()` now returns a data
frame with model names in a `model` column instead of a matrix with names in
the row names. The package now handles both output formats.

## Test environments

* local Mac OSX install, R 4.4.1
* Windows, macOS, Ubuntu 22.04.1 on Github actions (devel and release)
* win-builder (devel and release)


## R CMD check results

0 errors | 0 warnings | 0 note


## Note

