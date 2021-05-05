# wrapper for avoiding build version mismatches when querying Bioc
deepdep_wrapped <- function(...) {
  tryCatch(deepdep(...),
           error = function(err) {
             if (grepl("Bioconductor version '[0-9\\.]*' requires R version '[0-9\\.]*'; R version is too new;.*",
                       err$message))
               skip("Build's Bioconductor and R version mismatch makes performing test impossible")
             else stop(err$message)
           })
  }

vcr::use_cassette("deepdep-1", {
  test_that("obtaining local dependencies returns objects of correct types", {
    dd <- deepdep("deepdep", local = TRUE, depth = 1, dependency_type = c("Depends", "Imports", "Enhances", "LinkingTo"))
    expect_is(dd, "deepdep")
  })
})

vcr::use_cassette("deepdep-2", {
  
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

vcr::use_cassette("deepdep-3", {
  test_that("packages with no dependencies return empty deepdep object in result", {
    dd <- deepdep("rlang")
    expect_equal(nrow(dd), 0)
  })
})
