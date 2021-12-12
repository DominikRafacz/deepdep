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
#' @param dependency_type A \code{character} vector. Types of the dependencies that should be sought, a subset of
#' \code{c("Imports", "Depends", "LinkingTo", "Suggests", "Enhances")}. Other possibilities are: character string
#' \code{"all"}, a shorthand for the whole vector; character string \code{"most"} for the same vector without \code{"Enhances"};
#' character string \code{"strong"} (default) for the first three elements of that vector. Works analogously to
#' \code{\link[tools]{package_dependencies}}.
#' @param version A \code{character}. Name of the version published on CRAN that should be retrieved.
#' Does not work for Bioconductor or local packages. Defaults to \code{NULL} which means 'get the latest version available'.
#'
#' @return An object of \code{deepdep} class.
#'
#' @seealso \code{\link{get_dependencies}}
#'
#' @examples
#'
#' \donttest{
#' library(deepdep)
#'
#' dd_downloads <- deepdep("ggplot2")
#' head(dd_downloads)
#'
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
                    dependency_type = "strong", version = NULL) {

  check_package_name(package, bioc, local)

  pkg_dep <- get_dependencies(package, downloads, bioc, local, dependency_type, version)

  # check if there are any dependencies
  if (length(pkg_dep$name) > 0) {
    ret <- data.frame(origin = package, pkg_dep, origin_level = 0, dest_level = 1)
  } else {
    ret <- data.frame(origin = character(), pkg_dep, origin_level = numeric(), dest_level = numeric())
  }

  if (depth > 1) {
    already_subdeped <- package
    to_subdep <- pkg_dep$name

    for (level in 2:depth) {
      added <- do.call(rbind, lapply(to_subdep, function(pkg_name) {
        pkg_subdep <- get_dependencies(pkg_name, downloads, bioc, local, dependency_type)
        if (length(pkg_subdep$name) > 0) {
          upper_layer_origin <- ret[match(pkg_subdep$name, ret$origin), "origin_level"]
          upper_layer_dest <- ret[match(pkg_subdep$name, ret$name), "dest_level"]
          cb <- ifelse(is.na(upper_layer_origin),
                       ifelse(is.na(upper_layer_dest), level,
                              upper_layer_dest),
                       upper_layer_origin)

          data.frame(origin = pkg_name,
                     pkg_subdep,
                     origin_level = level - 1,
                     dest_level = cb)
        } else NULL
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
#' \donttest{
#' library(deepdep)
#'
#' dd <- deepdep("stringr")
#' dd
#' }
#'
#' @rdname print.deepdep
#' @export
print.deepdep <- function(x, ...) {
  print.data.frame(x)
}
