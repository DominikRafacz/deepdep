# wrapper for avoiding build version mismatches when querying Bioc
deepdep_wrapped <- function(...) {
  tryCatch(deepdep(...),
           error = function(err) {
             if (grepl("Bioconductor version '[0-9\\.]*' requires R version '[0-9\\.]*'",
                       err$message) ||
                 grepl("Bioconductor does not yet build and check packages for R version [0-9\\.]*",
                       err$message))
               skip("Build's Bioconductor and R version mismatch makes performing test impossible")
             else stop(err$message)
           })
  }

test_that("obtaining local dependencies returns objects of correct types", {
  dd <- deepdep("deepdep", local = TRUE, depth = 1, dependency_type = c("Depends", "Imports", "LinkingTo", "Suggests"))
  expect_is(dd, "deepdep")
})

test_that("using shorthand parameters returns the same result", {
  dd <- deepdep("deepdep", local = TRUE, depth = 1, dependency_type = c("Depends", "Imports", "LinkingTo", "Suggests"))
  dd2 <- deepdep("deepdep", local = TRUE, depth = 1, dependency_type = "most")
  expect_identical(dd, dd2)
})

reset_cached_files("ava")
reset_cached_files("deps")
reset_cached_files("desc")

vcr::use_cassette("deepdep-1", {
  test_that("obtaining Bioc dependencies returns objects of correct types", {
    dd <- deepdep_wrapped("les", downloads = FALSE, bioc = TRUE, depth = 1, dependency_type = c("Depends", "Imports", "Enhances", "LinkingTo"))
    expect_is(dd, "deepdep")
  })
})


test_that("incorrect combination of parameters results in error",{
  expect_error(deepdep_wrapped("ggforce", downloads = TRUE, local =  FALSE, bioc = TRUE, depth = 2, dependency_type = c("Depends", "Imports")))
  expect_error(deepdep_wrapped("ggforce", downloads = TRUE, local =  TRUE, bioc = TRUE, depth = 2, dependency_type = c("Depends", "Imports")))
  expect_error(deepdep_wrapped("ggforce", downloads = FALSE, local =  TRUE,  bioc = TRUE, depth = 2, dependency_type = c("Depends", "Imports")))
  expect_error(deepdep("ggforce", downloads = TRUE, local =  TRUE, bioc = FALSE, depth = 2, dependency_type = c("Depends", "Imports")))
})

reset_cached_files("ava")
reset_cached_files("deps")
reset_cached_files("desc")

vcr::use_cassette("deepdep-2", {
  test_that("packages with no dependencies return empty deepdep object in result", {
    dd <- deepdep("rlang")
    expect_equal(nrow(dd), 0)
  })
})
