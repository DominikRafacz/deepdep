context("get_downloads")

test_that("Test get download and print", {
  downloads <- get_downloads("DALEX")
  expect_is(downloads, "package_downloads")
  expect_error(print(downloads), NA)
})
