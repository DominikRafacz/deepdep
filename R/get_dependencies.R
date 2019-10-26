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
#' dependencies <- get_dependencies("stringr")
#' dependencies
#' 
#'
#' @export
get_dependencies <- function(package, downloads = FALSE) {
  
  if (!is_available(package)) return(NULL)
  
  description <- get_description(package)
  
  package_names <- names(c(description$depends, description$imports, description$suggests,
                           description$enhances, description$linkingto))
  
  package_versions <- unlist(c(description$depends, description$imports, description$suggests,
                               description$enhances, description$linkingto), use.names = FALSE)
  
  package_types <- c(rep("Depends", length(description$depends)), rep("Imports", length(description$imports)),
                     rep("Suggests", length(description$suggests)), rep("Enhances", length(description$enhances)),
                     rep("LinkingTo", length(description$linkingto)))
    
  if (!is.null(package_names)) {
    downloads_df <- NULL
    remove_base_or_R <- sapply(package_names, is_available)
    
    if (downloads) {
      downloads_list <- lapply(package_names, get_downloads)
      downloads_df <- as.data.frame(do.call(rbind, downloads_list))
    }
    
    # this works if downloads_df is NULL
    ret <- as.data.frame(cbind(name = package_names[remove_base_or_R],
                               version = package_versions[remove_base_or_R],
                               type = package_types[remove_base_or_R],
                               downloads_df),
                         stringsAsFactors = FALSE)
    
    ret$downloads_df <- downloads_df  
  } else return(NULL)
  
  attr(ret, "package_name") <- package
  class(ret) <- c("package_dependencies", "data.frame")
  ret
}

#' @title Print function for an object of \code{package_dependencies} class
#' 
#' @param x An object of \code{package_dependencies} class.
#' @param ... other
#'
#' @author Hubert Baniecki, Szymon Maksymiuk
#' 
#' @examples
#' library(deepdep)
#' 
#' dependencies <- get_dependencies("stringr")
#' dependencies
#' 
#' @rdname print.package_dependencies
#' @export
print.package_dependencies <- function(x, ...) {
  print.data.frame(x)
}
