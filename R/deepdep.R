#' @title deepdep
#' 
#' @description deepdep. 
#' 
#' @param package A \code{character}. Name of the package that is on CRAN. 
#' @param downloads A \code{logical}. If \code{TRUE} add package downloads data. By default it's \code{FALSE}.
#' @param depth An \code{integer}. Depth of package dependency. By default it's \code{1}.
#' 
#' @return An object of \code{deepdep} class. 
#' 
#' @author Hubert Baniecki, Szymon Maksymiuk
#' 
#' @examples 
#' library(deepdep)
#' 
#' dd <- deepdep("stringr", downloads = TRUE)
#' dd
#' 
#' dd2 <- deepdep("stringr", depth = 2)
#' dd2
#' 
#' @export
deepdep <- function(package, downloads = FALSE, depth = 1) {
  
  check_package_name(package)
  
  pkg_dep <- get_dependencies(package, downloads)
  pkg_dep_names <- pkg_dep$name
  
  ret <- data.frame(origin = attr(pkg_dep, "package_name"), pkg_dep)
  
  pkg_dep_dep_names <- c()
  already_computed_names <- c(package)
  
  if (depth > 1) {
    for (i in 2:depth) {
      for (name in pkg_dep_names) {
        pkg_dep_dep <- get_dependencies(name, downloads)
        
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
#' @author Hubert Baniecki, Szymon Maksymiuk
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