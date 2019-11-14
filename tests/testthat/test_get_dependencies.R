context("get_dependencies")

test_that("Check for dependencies", {
  expect_error(get_dependencies("DALEX", deps_types = c("Nonallowedtype", "Depends")))
})

test_that("CRAN packages work", {
  deps1 <- get_dependencies("mltools")
  expect_is(deps1, "package_dependencies")
})


test_that("CRAN packages work, without downloads", {
  deps2 <- get_dependencies("mltools", downloads = FALSE)
  expect_is(deps2, "package_dependencies")
})

test_that("Bioc packages work", {
  deps3 <- get_dependencies("a4", bioc = TRUE, downloads = FALSE)
  expect_is(deps3, "package_dependencies")
})

test_that("Local packages work", {
  deps4 <- get_dependencies("deepdep", local = TRUE, downloads = FALSE, deps_types = c("Depends", "Imports"))
  expect_is(deps4, "package_dependencies")
})

test_that("Print works", {
  deps1 <- get_dependencies("mltools")
  expect_error(print(deps1), NA)
})
