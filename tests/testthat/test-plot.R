# OFFLINE TESTS ----
test_that("incorrect object type results in an error", {
  expect_error(plot_dependencies("Wrong type"))
})

# SETUP ----
# Tests below make HTTP calls, so we use vcr to record them
skip_if_not_installed("vcr")

# Write cassettes first
vcr::use_cassette("plot-dd-shiny", {
  dd_shiny <- deepdep("shiny", depth = 2)
})
vcr::use_cassette("plot-dd-rlang", {
  dd_rlang <- deepdep("rlang")
  plt_rlang <- plot_dependencies(dd_rlang)
})
vcr::use_cassette("plot-dd-datatable", {
  dd_dt <- deepdep("data.table", depth = 2, dependency_type = "all")
})

# LAYER CLASSES ----
test_that("deepdep plot has correct layers and objects", {
  plt_shiny <- plot_dependencies(dd_shiny)
  
  expect_length(plt_shiny$layers, 4)
  # 1st layer is circles
  expect_s3_class(plt_shiny$layers[[1]]$geom, "GeomPath")
  expect_s3_class(plt_shiny$layers[[1]]$stat, "StatIdentity")
  # 2nd layer is edges
  expect_s3_class(plt_shiny$layers[[2]]$geom, "GeomEdgePath")
  expect_s3_class(plt_shiny$layers[[2]]$stat, "StatEdgeLink")
  # 3rd layer is node points
  expect_s3_class(plt_shiny$layers[[3]]$geom, "GeomPoint")
  expect_s3_class(plt_shiny$layers[[3]]$stat, "StatFilter")
  # 4th layer is node labels
  expect_s3_class(plt_shiny$layers[[4]]$geom, "GeomLabel")
  expect_s3_class(plt_shiny$layers[[4]]$stat, "StatFilter")
})

# CAPTION ----
test_that("deepdep plot has a caption by default", {
  plt_shiny <- plot_dependencies(dd_shiny)
  
  expect_match(
    plt_shiny$labels$caption,
    "Plot made with deepdep v[\\d.]+ on \\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}",
    perl = TRUE
  )
  
  plt_shiny <- plot_dependencies(dd_shiny, show_stamp = TRUE)
  
  expect_match(
    plt_shiny$labels$caption,
    "Plot made with deepdep v[\\d.]+ on \\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}",
    perl = TRUE
  )
})

test_that("deepdep plot has no caption when specified", {
  plt_shiny <- plot_dependencies(dd_shiny, show_stamp = FALSE)
  
  expect_null(plt_shiny$labels$caption)
})

# NO DEPENDENCIES ----
test_that("deepdep plot with no dependencies has only a subset of layers", {
  # No circles, no edges
  # 1st layer is node points
  expect_s3_class(plt_rlang$layers[[1]]$geom, "GeomPoint")
  expect_s3_class(plt_rlang$layers[[1]]$stat, "StatFilter")
  # 2nd layer is node labels
  expect_s3_class(plt_rlang$layers[[2]]$geom, "GeomLabel")
  expect_s3_class(plt_rlang$layers[[2]]$stat, "StatFilter")
})

# DECLUTTER ----
test_that("declutter removes all Suggests and Enhances packages from outer layers", {
  plt_dt <- plot_dependencies(dd_dt, declutter = TRUE)
  
  # Node labels are equal to the names appearing in the filtered data
  filtered_dd <- dd_dt[dd_dt$origin_level == 0 |
                         !dd_dt$type %in% c("Suggests", "Enhances"), ]
  packages_plotted <- unique(c(filtered_dd$origin, filtered_dd$name))
  expect_setequal(packages_plotted, plt_dt$data$name)
  
  # If edge has type Suggests or Enhances, it starts in the center
  edge_attrs <- igraph::edge_attr(attr(plt_dt$data, "graph"))
  expect_true(
    all(edge_attrs$origin_level[edge_attrs$type %in% c("Suggests", "Enhances")] == 0)
  )
})
