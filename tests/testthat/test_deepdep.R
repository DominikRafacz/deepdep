context("deepdep")

test_that("deepdep executes with local = FALSE", {
  dd <- deepdep("RcppArmadillo", downloads = TRUE, depth = 1, dependency_type = c("Depends", "Imports", "Enhances", "LinkingTo"))
  dd3 <- deepdep("RcppArmadillo", downloads = FALSE, depth = 1, dependency_type = c("Depends", "Imports", "Enhances", "LinkingTo"))
  dd5 <- deepdep("ggforce", downloads = FALSE, depth = 1, dependency_type = c("Depends", "Imports", "Enhances", "LinkingTo"))
  expect_is(dd3, "deepdep")
  expect_is(dd5, "deepdep")
  expect_is(dd, "deepdep")
})


 test_that("deepdep exacutes with bio = TRUE", {
   dd2 <- deepdep("les", downloads = FALSE, bioc = TRUE, depth = 1, dependency_type = c("Depends", "Imports", "Enhances", "LinkingTo"))
   expect_is(dd2, "deepdep")
})

test_that("Error check",{
  expect_error(deepdep("ggforce", downloads = TRUE, local =  FALSE, bioc = TRUE, depth = 2, dependency_type = c("Depends", "Imports")))
  expect_error(deepdep("ggforce", downloads = TRUE, local =  TRUE, bioc = TRUE, depth = 2, dependency_type = c("Depends", "Imports")))
  expect_error(deepdep("ggforce", downloads = FALSE, local =  TRUE,  bioc = TRUE, depth = 2, dependency_type = c("Depends", "Imports")))
  expect_error(deepdep("ggforce", downloads = TRUE, local =  TRUE, bioc = FALSE, depth = 2, dependency_type = c("Depends", "Imports")))
})

test_that("Packages with no dependencies give empty deepdep object in result", {
  dd <- deepdep("rlang")
  expect_equal(nrow(dd), 0)
})
