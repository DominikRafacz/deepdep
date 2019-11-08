#' @title Get available packages
#' 
#' @description Obtain a vector of names of all packages that you have installed or that are
#' available to be installed.
#' 
#' @param bioc A \code{\link{logical}} value. Should only packages from Bioconductor repositories
#' be listed? For this option to work properly, \code{BiocManager} package needs to be 
#' installed.
#' @param local A \code{\link{logical}} value. Should only local packages (installed packages) be
#' listed?
#' @param reset_cache A \code{\link{logical}} value. Should cache be cleared before obtaining the 
#' list of packages?
#' 
#' @return A \code{\link{character}} vector. If neither \code{local} nor \code{bioc} are 
#' \code{TRUE}, vector contains all packages available currently on CRAN. If \code{bioc} is 
#' \code{TRUE}, vector contains all packages available currently via Bioconductor. If
#' \code{local} is \code{TRUE}, vactor contains all packages that are currently installed.
#' 
#' @details Function uses caching - only the first usage scraps information from servers. Those
#' objects are then saved locally in temporary file and further usages loads needed data from the
#' file.
#' 
#' Parameters \code{bioc} and \code{local} cannot be \code{TRUE} simultaneously.
#' 
#' @export
get_available_packages <- function(bioc = FALSE, local = FALSE, reset_cache = FALSE) {
  if (local && bioc) stop("You cannot use both 'local' and 'bioc' options at once.")
  if (reset_cache) reset_cached_files("ava")
  if (local) return(get_ava_cached("local"))
  if (bioc) get_ava_cached("bioc")
  else get_ava_cached("CRAN")
}

#' @importFrom utils available.packages contrib.url
#'
get_ava_cached <- function(repo) {
  pkgs <- get_cached_obj("ava", repo)
  if (attr(pkgs, "new")) {
    pkgs <- switch(repo,
                   CRAN = available.packages(
                     contriburl = contrib.url("https://cloud.r-project.org/"))[, 1],
                   bioc = {check_bioc_installed()
                     BiocManager::available()},
                   local = stop("NOT IMPLEMENTED YET")) #here should go code listing installed packages or sthng
    attr(pkgs, "type") <- "ava"
    attr(pkgs, "repo") <- "bioc"
    attr(pkgs, "new") <- FALSE
    save_cache(pkgs)
  }
  pkgs
}

check_bioc_installed <- function() {
  ### IMPORTANT NOTE: until local option is not implemented, this function in fact does not work
  # at all - after implementing, parameter local should be changed to TRUE
  if (!is_available("BiocManager", local = FALSE)) 
    stop("You cannot use 'bioc' option if you don't have 'BiocManager' package installed")
}

is_available <- function(package, bioc = FALSE, local = FALSE) {
  package %in% get_available_packages(bioc, local)
}