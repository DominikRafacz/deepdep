skip_if_not_installed("vcr")

vcr::use_cassette("dd-declutr-R6", {
  dd_R6 <- deepdep("R6", depth = 3, dependency_type = "all")
})

test_that("number of rows doesn't increase", {
  expect_lte(nrow(deepdep_declutter(dd_R6)), nrow(dd_R6))
})

test_that("no Suggests or Enhances in outer layers", {
  dd_R6_dcl <- deepdep_declutter(dd_R6)
  
  expect_true(all(
    dd_R6_dcl[dd_R6_dcl[["type"]] %in% c("Suggests", "Enhances"), "origin_level"] == 0
  ))
})
