context("Test for development")

desc <- get_description("stringr")
testthat::expect_s3_class(desc, "package_description")

dep <- get_dependencies("stringr")
testthat::expect_s3_class(dep, "package_dependencies")

down <- get_downloads("stringr")
testthat::expect_s3_class(down, "package_downloads")

dd <- deepdep("stringr")
testthat::expect_s3_class(dd, "deepdep")

get_dep_without_dependencies <- get_dependencies("abc.data")
testthat::expect_named(get_dep_without_dependencies,
                       expected = c("name", "version", "type"))
