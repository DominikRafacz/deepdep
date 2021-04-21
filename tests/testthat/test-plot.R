vcr::use_cassette("plot-1", {
  deps <- deepdep("shiny", depth = 2)
})

test_that("dependencies plot has correct layers and objects", {
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

test_that("incorrect object type results in an error", {
  expect_error(plot_dependencies("Wrong type"))
})


vcr::use_cassette("plot-2", {
  test_that("plotting deepdep object with no rows results in less layers", {
    plt <- plot_dependencies("rlang")
    expect_s3_class(plt$layers[[1]]$geom, "GeomPoint")
    expect_s3_class(plt$layers[[1]]$stat, "StatFilter")
    expect_s3_class(plt$layers[[2]]$geom, "GeomLabel")
    expect_s3_class(plt$layers[[2]]$stat, "StatFilter")

  })
})
