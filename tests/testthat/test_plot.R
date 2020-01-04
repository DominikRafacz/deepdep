context("Tests for plots")

deps <- deepdep("ttdo", depth = 2)


test_that("dependencies plot have suitable layers and objects", {
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

test_that("Type checks", {
  expect_error(plot_dependencies("Wrong type"))
})

test_that("Tree graph test", {
  plt2 <- plot_dependencies(deps, type = "tree")
  expect_s3_class(plt2, "ggraph")
})

test_that("Plotting deepdep object with no rows works correctly", {
  plt <- plot_dependencies("rlang")
  expect_s3_class(plt$layers[[1]]$geom, "GeomPoint")
  expect_s3_class(plt$layers[[1]]$stat, "StatFilter")
  expect_s3_class(plt$layers[[2]]$geom, "GeomLabel")
  expect_s3_class(plt$layers[[2]]$stat, "StatFilter")

})
