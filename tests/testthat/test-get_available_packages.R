vcr::use_cassette("get_available_packages-1", {
  test_that("getting available packages from CRAN returns no errors", {
    expect_error(get_available_packages(), NA)
  })
})

vcr::use_cassette("get_available_packages-2", {
  test_that("getting available packages from both local and bioc returns an error", {
    expect_error(get_available_packages(local = TRUE, bioc = TRUE),
                 "You cannot use both 'local' and 'bioc' options at once.")
  })
})

vcr::use_cassette("get_available_packages-3", {
  test_that("chache is cleared without errors", {
    expect_error(get_available_packages(local = TRUE, reset_cache = TRUE), NA)
  })
})

