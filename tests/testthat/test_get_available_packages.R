context("get_available_packages")

test_that("Description CRAN evoks without any error", {
  expect_error(get_available_packages(), NA)
})

# test_that("Description bioc evoks without any error", {
#   expect_error(get_available_packages(bioc = TRUE), NA)
# })

test_that("Description CRAN evoks without any error", {
  expect_error(get_available_packages(local = TRUE), NA)
})

test_that("Error occurs", {
  expect_error(get_available_packages(local = TRUE, bioc = TRUE))
})

test_that("Cache cleares without errors", {
  expect_error(get_available_packages(local = TRUE, reset_cache = TRUE), NA)
})

