test_that("description object has correct class", {
  desc <- get_description("deepdep", local = TRUE)
  testthat::expect_s3_class(desc, "package_description")
})

test_that("dependencies object has correct class", {
  dep <- get_dependencies("deepdep", local = TRUE, downloads = FALSE)
  testthat::expect_s3_class(dep, "package_dependencies")
})

test_that("deepdep object has correct class", {
  dd <- deepdep("deepdep", local = TRUE, )
  testthat::expect_s3_class(dd, "deepdep")
})


