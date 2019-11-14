#' @title Acquire the dependencies of the package on any depth level
#'
#' @description This function is an ultimate wrapper for \code{\link{get_dependencies}}. It inherits all of the arguments and
#' allows to recursively search for the dependencies at the higher level of \code{depth}.
#'
#' @param package A \code{character}. Name of the package that is on CRAN, Bioconductor repository or locally installed.
#' See \code{bioc} and \code{local} arguments.
#' @param depth An \code{integer}. Maximum depth level of the dependency. By default it's \code{1}.
#' @param downloads A \code{logical}. If \code{TRUE} add dependency downloads data. By default it's \code{FALSE}.
#' @param bioc A \code{logical} value. If \code{TRUE} the Bioconductor dependencies data will be taken from.
#' Bioconductor repository. For this option to work properly, \code{BiocManager} package needs to be installed.
#' @param local A \code{logical} value. If \code{TRUE} only data of locally installed packages will be used (without API usage).
#' @param deps_types A \code{character} vector. Types of the dependencies that should be sought.
#' Possibilities are: \code{"Imports", "Depends", "Suggests", "Enhances", "LinkingTo"}. By default it's \code{"Depends", "Imports"}.
#'
#' @return An object of \code{deepdep} class.
#'
#' @seealso \code{\link{get_dependencies}}
#'
#' @examples
#' library(deepdep)
#'
#' dd_downloads <- deepdep("ggplot2", downloads = TRUE)
#' head(dd_downloads)
#'
#' dd_2 <- deepdep("ggplot2", depth = 2)
#' plot_dependencies(dd_2, "circular")
#'
#' \dontrun{
#' dd_local <- deepdep("deepdep", local = TRUE)
#' plot_dependencies(dd_local)
#' }
#'
#'
#' @export
deepdep <- function(package, depth = 1, downloads = FALSE, bioc = FALSE, local = FALSE,
                    deps_types = c("Depends", "Imports")) {

  check_package_name(package, bioc, local)

  pkg_dep <- get_dependencies(package, downloads, bioc, local, deps_types)
  pkg_dep_names <- pkg_dep$name

  ret <- data.frame(origin = attr(pkg_dep, "package_name"), pkg_dep)

  pkg_dep_dep_names <- c()
  already_computed_names <- c(package)

  if (depth > 1) {
    for (i in 2:depth) {
      for (name in pkg_dep_names) {
        pkg_dep_dep <- get_dependencies(name, downloads, bioc, local, deps_types)

        if (length(pkg_dep_dep$name) != 0) {
          # find all unique dependency names (for next depth level)
          pkg_dep_dep_names <- union(pkg_dep_dep_names, pkg_dep_dep$name)
          ret <- rbind(ret, cbind(origin = attr(pkg_dep_dep, "package_name"), pkg_dep_dep))
        }
      }

      # save all package names that were already computed
      already_computed_names <- union(already_computed_names, pkg_dep_dep_names)

      # find all package names that were not computed yet
      pkg_dep_names <- intersect(already_computed_names, pkg_dep_dep_names)
      pkg_dep_dep_names <- c()
    }
  }

  attr(ret, "package_name") <- package
  class(ret) <- c("deepdep", "data.frame")
  ret
}

#' @title Print function for an object of \code{deepdep} class
#'
#' @param x An object of \code{deepdep} class.
#' @param ... other
#'
#'
#' @examples
#' library(deepdep)
#'
#' dd <- deepdep("stringr")
#' dd
#'
#' @rdname print.deepdep
#' @export
print.deepdep <- function(x, ...) {
  print.data.frame(x)
}
