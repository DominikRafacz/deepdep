#' @title Acquire the dependencies of the package on any depth level
#'
#' @description This function is an ultimate wrapper for \code{\link{get_dependencies}}. It inherits all of the arguments and
#' allows to recursively search for the dependencies at the higher level of \code{depth}.
#'
#' @param package A \code{character}. Name of the package that is on CRAN, Bioconductor repository or locally installed.
#' See \code{bioc} and \code{local} arguments.
#' @param depth An \code{integer}. Maximum depth level of the dependency. By default it's \code{1}.
#' @param downloads A \code{logical}. If \code{TRUE} add dependency downloads data. By default it's \code{FALSE}.
#' @param bioc A \code{logical} value. If \code{TRUE} the Bioconductor dependencies data will be taken from the
#' Bioconductor repository. For this option to work properly, \code{BiocManager} package needs to be installed.
#' @param local A \code{logical} value. If \code{TRUE} only data of locally installed packages will be used (without API usage).
#' @param dependency_type A \code{character} vector. Types of the dependencies that should be sought.
#' Possibilities are: \code{"Imports", "Depends", "Suggests", "Enhances", "LinkingTo"}. By default it's \code{"Depends", "Imports"}.
#'
#' @return An object of \code{deepdep} class.
#'
#' @seealso \code{\link{get_dependencies}}
#'
#' @examples
#' library(deepdep)
#'
#' dd_downloads <- deepdep("ggplot2")
#' head(dd_downloads)
#'
#' \donttest{
#' dd_2 <- deepdep("ggplot2", depth = 2, downloads = TRUE)
#' plot_dependencies(dd_2, "circular")
#'
#' dd_local <- deepdep("deepdep", local = TRUE)
#' plot_dependencies(dd_local)
#' }
#'
#'
#' @export
deepdep <- function(package, depth = 1, downloads = FALSE, bioc = FALSE, local = FALSE,
                    dependency_type = c("Depends", "Imports")) {

  check_package_name(package, bioc, local)

  pkg_dep <- get_dependencies(package, downloads, bioc, local, dependency_type)
  pkg_dep_names <- pkg_dep$name

  # check if there are any dependencies
  if (length(pkg_dep_names)) {
    ret <- data.frame(origin = attr(pkg_dep, "package_name"), pkg_dep)
  } else {
    ret <- data.frame(origin = character(), pkg_dep)
  }

  if (depth > 1) {
    already_subdeped <- package
    to_subdep <- pkg_dep_names

    for (i in 2:depth) {
      added <- do.call(rbind, lapply(to_subdep, function(pkg_name) {
        pkg_subdep <- get_dependencies(pkg_name, downloads, bioc, local, dependency_type)
        if (length(pkg_subdep$name) > 0)
          data.frame(origin = attr(pkg_subdep, "package_name"), pkg_subdep)
        else
          NULL
      }))

      already_subdeped <- union(already_subdeped, to_subdep)
      to_subdep <- setdiff(added$name, already_subdeped)
      ret <- rbind(ret, added)
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
