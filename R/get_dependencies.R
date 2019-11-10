#' @title Acquire dependencies of the package 
#' 
#' @description This function is using \code{\link{get_description}} and \code{\link{get_downloads}}
#' to acquire dependencies.
#' 
#' @param package A \code{character}. Name of the package that is on CRAN.
#' @param downloads A \code{logical}. If \code{TRUE} add package downloads data. By default it's \code{FALSE}.
#' @param bioc A \code{logical} value. Should Bioconductor dependencies descriptions be red from 
#' Bioconductor repository? For this option to work properly, \code{BiocManager} package needs to be 
#' installed.
#' @param local A \code{logical} value. Should only already installed packages be checked?
#' @param deps_types A \code{character} vector. Types of dependencies that should be sought.
#' Possibilities are: Imports, Depends, Suggests, Enhances, LinkingTo. Defaults to "Depends" and "Imports".
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
get_dependencies <- function(package, downloads = FALSE, bioc = FALSE, local = FALSE,
                             deps_types = c("Depends", "Imports")) {
  if (downloads && (local || bioc)) stop("If you use downloads, you cannot use",
                                         " neither bioc nor local")
  
  possible_types <- c("Depends", "Imports", "Suggests", "Enhances", "LinkingTo")
  
  deps_types <- unique(deps_types)
  if (!all(deps_types %in% possible_types) || length(deps_types) < 1) 
    stop("'deps_types' should specify which types of dependencies should be included")
  l_deps_types <- tolower(deps_types)
  names(deps_types) <- l_deps_types
  
  description <- get_description(package, bioc, local)
  
  deps <- description[l_deps_types]
  
  package_names <- unlist(sapply(deps, names))
  package_versions <- unlist(deps, use.names = FALSE)
  package_types <- unlist(sapply(names(deps), function(dep_type) 
    rep(deps_types[dep_type], length(deps[[dep_type]]))))
    
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
    # deletes downloads_df column if ret is empty
    if (is.null(downloads_df)) {
      ret$downloads_df <- downloads_df
    }
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
