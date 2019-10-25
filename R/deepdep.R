#' deepdep
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
#' dd <- deepdep("ggplot2")
#' dd
#' 
#' dd5 <- deepdep("ggplot2")
#' dd5
#' 
#' @export
deepdep <- function(package, downloads = FALSE, depth = 1) {
  
  check_package_name(package)
  
  package_dependencies <- get_dependencies(package, downloads)
  
  if (depth > 1) {
    for (i in 2:depth) {
      x <- lapply(package_dependencies$name, function(x) get_dependencies(x, downloads))
    }
  }
  
  # cbind with 'from' package name (package_name attr)
  # TODO: make df
  # rbind
  
  class(ret) <- c("deepdep", "data.frame")
  ret
}

#' @export
print.deepdep <- function(x, ...) {
  print.data.frame(x)
}