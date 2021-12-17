test_that("dependencies plot has correct classes and attributes", {
  skip_if_not_installed("vcr")
  
  reset_cached_files("ava")
  reset_cached_files("deps")
  reset_cached_files("desc")
  
  skip("Cassette unusable due to encoding issue in vcr (check vcr issue #240).")
  
  # TODO: Use cassette again when this issue is solved:
  #  https://github.com/ropensci/vcr/issues/240
  vcr::use_cassette("plot-dnl-1", {
    deps <- deepdep("xgboost", dependency_type = "Imports")
    plt <- plot_downloads(deps, from = as.Date("2020-07-01"), to = as.Date("2021-06-30"))
  })
  
  # Is returned value a ggplot2 plot?
  expect_s3_class(plt, c("gg", "ggplot"))
  # Does title contain package name?
  expect_true(grepl("xgboost", plt$labels$title))
})
