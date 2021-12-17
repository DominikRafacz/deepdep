test_that("incorrect object type results in an error", {
  expect_error(plot_dependencies("Wrong type"))
})

# Tests below make HTTP calls, so we use vcr to record them
skip_if_not_installed("vcr")

test_that("dependencies plot has correct layers and objects", {
  reset_cached_files("ava")
  reset_cached_files("deps")
  reset_cached_files("desc")
  
  vcr::use_cassette("plot-1", {
    deps <- deepdep("shiny", depth = 2)
  })
  
  plt <- plot_dependencies(deps)
  expect_length(plt$layers, 4)
  expect_s3_class(plt$layers[[1]]$geom, "GeomPath")
  expect_s3_class(plt$layers[[1]]$stat, "StatIdentity")
  expect_s3_class(plt$layers[[2]]$geom, "GeomEdgePath")
  expect_s3_class(plt$layers[[2]]$stat, "StatEdgeLink")
  expect_s3_class(plt$layers[[3]]$geom, "GeomPoint")
  expect_s3_class(plt$layers[[3]]$stat, "StatFilter")
  expect_s3_class(plt$layers[[4]]$geom, "GeomLabel")
  expect_s3_class(plt$layers[[4]]$stat, "StatFilter")
})

test_that("plotting deepdep object with no rows results in less layers", {
  reset_cached_files("ava")
  reset_cached_files("deps")
  reset_cached_files("desc")
  
  vcr::use_cassette("plot-2", {
    plt <- plot_dependencies("rlang")
  })
  
  expect_s3_class(plt$layers[[1]]$geom, "GeomPoint")
  expect_s3_class(plt$layers[[1]]$stat, "StatFilter")
  expect_s3_class(plt$layers[[2]]$geom, "GeomLabel")
  expect_s3_class(plt$layers[[2]]$stat, "StatFilter")
})

test_that("decluttering removes all Suggests and Enhances packages from outer layers", {
  reset_cached_files("ava")
  reset_cached_files("deps")
  reset_cached_files("desc")
  
  vcr::use_cassette("deepdep-3", {
    dd <- deepdep("data.table", depth = 2, dependency_type = "all")
  })
  plt <- plot_dependencies(dd, depth = 2, declutter = TRUE,
                           dependency_type = "all")
  
  filtered_dd <- dd[dd$origin_level == 0 | !dd$type %in% c("Suggests", "Enhances"), ]
  packages_plotted <- unique(c(filtered_dd$origin, filtered_dd$name))
  
  expect_setequal(packages_plotted, plt$data$name)
  
  edge_attrs <- igraph::edge_attr(attr(plt$data, "graph"))
  expect_true(
    all(edge_attrs$origin_level[edge_attrs$type %in% c("Suggests", "Enhances")] == 0)
  )
})
