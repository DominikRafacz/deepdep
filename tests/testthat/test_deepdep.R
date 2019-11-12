context("deepdep")

test_that("deepdep executes with local = FALSE", {
  dd <- deepdep("RcppArmadillo", downloads = TRUE, depth = 1, deps_types = c("Depends", "Imports", "Enhances", "LinkingTo"))
  dd3 <- deepdep("RcppArmadillo", downloads = FALSE, depth = 1, deps_types = c("Depends", "Imports", "Enhances", "LinkingTo"))
  expect_is(dd3, "deepdep")
  expect_is(dd, "deepdep")
})

test_that("deepdep exacutes with local = TRUE, depth higheer than 1", {
  dd1 <- deepdep("ggforce", downloads = FALSE, local =  TRUE, depth = 2, deps_types = c("Depends", "Imports", "Enhances", "LinkingTo"))
  expect_is(dd1, "deepdep")
})

# test_that("deepdep exacutes with bio = TRUE", {
#   #want to clear cache
#   dep <- get_description("les", bioc = TRUE, reset_cache = TRUE)
#   dd2 <- deepdep("les", downloads = FALSE, bioc = TRUE, depth = 1, deps_types = c("Depends", "Imports", "Enhances", "LinkingTo"))
#   expect_is(dd2, "deepdep")
# })

test_that("Error check",{
  expect_error(deepdep("ggforce", downloads = TRUE, local =  FALSE, bioc = TRUE, depth = 2, deps_types = c("Depends", "Imports")))
  expect_error(deepdep("ggforce", downloads = TRUE, local =  TRUE, bioc = TRUE, depth = 2, deps_types = c("Depends", "Imports")))
  expect_error(deepdep("ggforce", downloads = FALSE, local =  TRUE,  bioc = TRUE, depth = 2, deps_types = c("Depends", "Imports")))
  expect_error(deepdep("ggforce", downloads = TRUE, local =  TRUE, bioc = FALSE, depth = 2, deps_types = c("Depends", "Imports")))
})
