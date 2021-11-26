test_that("using shorthand words works", {
  expect_identical(match_dependency_type("all"), c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances"))
  expect_identical(match_dependency_type("most"), c("Depends", "Imports", "LinkingTo", "Suggests"))
  expect_identical(match_dependency_type("strong"), c("Depends", "Imports", "LinkingTo"))
})

test_that("correct vector passes without a change", {
  vec <- c("Depends", "Suggests", "Enhances")
  expect_identical(vec, match_dependency_type(vec))
})

test_that("empty vector results in an error", {
  expect_error(match_dependency_type(c()), "'dependency_type' should specify which types of dependencies should be included")
})

test_that("incorrect word results in an error", {
  expect_error(match_dependency_type("randomword"), "'dependency_type' should specify which types of dependencies should be included")
  expect_error(match_dependency_type(c("Imports", "Depends", "Enchants")), "'dependency_type' should specify which types of dependencies should be included")
})
