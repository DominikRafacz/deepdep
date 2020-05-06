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

