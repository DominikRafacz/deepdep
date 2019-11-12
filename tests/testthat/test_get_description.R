context("get_description")

test_that("Getting CRAN description", {
  desc1 <- get_description("DALEX")
  expect_is(desc1, "package_description")
})

# test_that("Getting bioc description", {
#   desc2 <- get_description("a4", bioc = TRUE)
#   expect_is(desc2, "package_description")
# })

test_that("Getting local description", {
  desc3 <- get_description("deepdep", local = TRUE)
  expect_is(desc3, "package_description")
})

test_that(("Clearing cache"), {
  desc1 <- get_description("DALEX", reset_cache = TRUE)
  expect_is(desc1, "package_description")
})

test_that("Print description works", {
  desc4 <- get_description("cranlogs")
  expect_error(print(desc4), NA)
})

test_that("Test if availability works", {
  expect_null(get_description("Packaage that is no on bioc", bioc = TRUE))
})
