#' @title Check if the package name is valid
#'
#' @param package package name
#' @param bioc TRUE/FALSE
#' @param local TRUE/FALSE
#'
#' @noRd
check_package_name <- function(package, bioc, local) {
  if (local) {
    if (!is_available(package, local = TRUE)) stop(paste0(package, " is not installed"))
  }
  else if (!is_available(package, bioc)) {
    if (bioc) stop(paste0(package, " is not available neither on CRAN nor on Bioconductor"))
    else stop(paste0(package, " is not available on CRAN"))
  }
}

#' @title Check if package version exists on CRAN
#' 
#' @param package package name
#' @param version a single string
#' 
#' @noRd
#' 
#' @importFrom pkgsearch cran_package_history
check_package_version <- function(package, version) {
  if (!is.null(version) && 
      !version %in% cran_package_history(package)[["Version"]])
    stop(paste0("No version named '", version, "' for package ", package, " available on CRAN."))
}

is_available <- function(package, bioc = FALSE, local = FALSE) {
  package %in% get_available_packages(bioc, local)
}
