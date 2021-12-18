# deepdep (development version)
* fixed bug where `plot_dependencies()` would sometimes raise an error when using `show_version = TRUE` and `depth >= 2`
* fixed bug where `plot_dependencies()` would plot one non-central label even when using `label_percentage = 0`
* tests that use CRAN download counts now skip on CRAN only

# deepdep 0.4.0
* removed hard dependencies on plotting-related packages; `ggplot2`, `ggraph`, `graphlayouts`, `igraph`, and `scales` are now Suggests instead of Imports (issue #32)
* removed unused dependency on `ggforce`
* added `declutter` parameter for `plot_dependencies()` that allows for ignoring non-strong dependencies of Suggests while plotting (issue #27)
* `vcr` package is now optional for running tests (pull request #26)

# deepdep 0.3.1
* adjusted parameter of `deepdep` function similarly to `get_dependencies` (issue #24)

# deepdep 0.3.0
* adjusted `dependency_type` argument to be consistent with `tools` package (issue #19)
* added `show_stamp` parameter for plot function (issue #20)

# deepdep 0.2.5
* wrapped `deepdep` in tests with `tryCatch` in case of R-devel and Bioc-devel versions mismatch
* marked all non-local examples with "donttest"
* adjusted to new version of the `vcr` package

# deepdep 0.2.2 -- 0.2.4 
* this versions are attempts to eliminating all possible errors during checks associated with problems with internet connection:  this consists of marking time-consuming examples by "donttest" in the documentation, using the `vcr` package to mock up api queries and creating pre-compiled vignettes

# deepdep 0.2.1
* fixed an error in `deepdep` that caused dependencies of one package to be added to the result many times
* the way in which dependency layers are created during plotting has been improved
* added columns in `deepdep` object, informing in which dependency layers the source and target packages are located
* allowed more than five layers of dependencies on plot created by `plot_dependencies()`
* fixed bugs caused by `plot_dependencies()` used with `label_percentage` parameter
* `plot_dependencies()` used with `label_percentage` parameter now shows small dots whenever package has not enough downloads to be plotted with label (instead of empty labels, as previously)

# deepdep 0.2.0
* CRAN version

# deepdep 0.1.1
* renamed `deps_types` argument to `dependency_type`
* added shiny app that generates `deepdep` plot

# deepdep 0.1.0
* first stable version of the `deepdep` package
* implemented functions: `deepdep()`, `get_available_packages()`, `get_dependencies()`,
`get_description()`, `get_downloads()`, `plot_dependencies()`

