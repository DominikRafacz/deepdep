test_that("getting available packages from both local and bioc returns an error", {
  expect_error(get_available_packages(local = TRUE, bioc = TRUE),
               "You cannot use both 'local' and 'bioc' options at once.")
})

test_that("chache is cleared without errors", {
  expect_error(get_available_packages(local = TRUE, reset_cache = TRUE), NA)
})

