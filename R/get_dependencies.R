#' @title Acquire dependencies of the package 
#' 
#' @description This function is using \code{\link{get_description}} and \code{\link{get_downloads}}
#' to acquire dependencies.
#' 
#' @param package A \code{character}. Name of the package that is on CRAN.
#' @param downloads A \code{logical}. If \code{TRUE} add package downloads data. By default it's \code{FALSE}.
#' 
#' @return An object of \code{package_dependencies} class. 
#' 
#' @author Hubert Baniecki, Szymon Maksymiuk
#' 
#' @examples
#' library(deepdep)
#' 
#' dependencies <- get_dependencies("ggplot2")
#' dependencies
#' 
#'
#' @export
get_dependencies <- function(package, downloads = FALSE) {
  
  check_package_name(package)
  
  description <- get_description(package)
  description$depends$R <- NULL
  
  package_names <- names(c(description$depends, description$imports, description$suggests,
                           description$enhances, description$linkingto))
  
  package_versions <- unlist(c(description$depends, description$imports, description$suggests,
                               description$enhances, description$linkingto), use.names = FALSE)
  
  package_types <- c(rep("Depends", length(description$depends)), rep("Imports", length(description$imports)),
                     rep("Suggests", length(description$suggests)), rep("Enhances", length(description$enhances)),
                     rep("LinkingTo", length(description$linkingto)))
    
  downloads_df <- NULL
  
  if (downloads) {
    downloads_list <- lapply(package_names, get_downloads)
    downloads_df <- as.data.frame(do.call(rbind, downloads_list)) 
  }
  
  # this works if downloads_df is NULL
  ret <- as.data.frame(cbind(name = package_names,
                             version = package_versions,
                             type = package_types,
                             downloads_df),
                       stringsAsFactors = FALSE)
  
  attr(ret, "package_name") <- package
  class(ret) <- c("package_dependencies", "data.frame")
  ret
}

#' @rdname get_dependencies
#' @export
print.package_dependencies <- function(x, ...) {
  print.data.frame(x)
}
