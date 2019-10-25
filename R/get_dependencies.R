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
  
  depends <- imports <- suggests <- enhances <- linkingto <- NULL
  
  if (!is.null(description$depends)) {
    package_names <- names(description$depends)
    package_names <- package_names[!(package_names %in% "R")]
    
    if (length(package_names) != 0) {
      package_names <- names(description$depends)
      package_versions <- unlist(description$depends, use.names = FALSE)
      
      downloads <- lapply(package_names, get_downloads) 
      downloads <- as.data.frame(do.call(rbind, downloads))
      
      depends <- cbind(name = package_names, version = package_versions, type = "Depends", downloads)
    }
  }
  
  if (!is.null(description$imports)) {
    package_names <- names(description$imports)
    package_versions <- unlist(description$imports, use.names = FALSE)
    
    downloads <- lapply(package_names, get_downloads) 
    downloads <- as.data.frame(do.call(rbind, downloads))
    
    imports <- cbind(name = package_names, version = package_versions, type = "Imports", downloads)
  }
  
  if (!is.null(description$suggests)) {
    package_names <- names(description$suggests)
    package_versions <- unlist(description$suggests, use.names = FALSE)
    
    downloads <- lapply(package_names, get_downloads) 
    downloads <- as.data.frame(do.call(rbind, downloads))
    
    suggests <- cbind(name = package_names, version = package_versions, type = "Suggests", downloads)
  }
  
  if (!is.null(description$enhances)) {
    package_names <- names(description$enhances)
    package_versions <- unlist(description$enhances, use.names = FALSE)
    
    downloads <- lapply(package_names, get_downloads) 
    downloads <- as.data.frame(do.call(rbind, downloads))
    
    enhances <- cbind(name = package_names, version = package_versions, type = "Enhances", downloads)
  }
  
  if (!is.null(description$linkingto)) {
    package_names <- names(description$linkingto)
    package_versions <- unlist(description$linkingto, use.names = FALSE)
    
    downloads <- lapply(package_names, get_downloads) 
    downloads <- as.data.frame(do.call(rbind, downloads))
    
    linkingto <- cbind(name = package_names, version = package_versions, type = "LinkingTo", downloads)
  }
  
  ret <- rbind(depends, imports, suggests, enhances, linkingto)
  
  class(ret) <- c("package_dependencies", "data.frame")
  ret
}

#' @rdname get_dependencies
#' @export
print.package_dependencies <- function(x, ...) {
  print.data.frame(x)
}
