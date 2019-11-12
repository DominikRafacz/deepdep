#' @title Get the list of available packages.
#'
#' @description Get names of packages that you have locally installed or that are available to be installed.
#'
#' @param bioc A \code{logical} value. Should only packages from Bioconductor repositories
#' be listed? For this option to work properly, \code{\link{BiocManager}} package needs to be
#' installed.
#' @param local A \code{logical} value. Should only local packages (installed packages) be
#' listed?
#' @param reset_cache A \code{logical} value. Should cache be cleared before obtaining the
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
#' Arguments \code{bioc} and \code{local} cannot be \code{TRUE} simultaneously.
#'
#' @export
get_available_packages <- function(bioc = FALSE, local = FALSE, reset_cache = FALSE) {
  if (local && bioc) stop("You cannot use both 'local' and 'bioc' options at once.")
  if (reset_cache) reset_cached_files("ava")
  if (local) return(get_available_packages_cached("local"))
  if (bioc) get_available_packages_cached("bioc")
  else get_available_packages_cached("CRAN")
}

#' @importFrom utils available.packages contrib.url
get_available_packages_cached <- function(repo) {
  pkgs <- get_cached_obj("ava", repo)
  if (attr(pkgs, "new")) {
    pkgs <- switch(repo,
                   CRAN = available.packages(
                     contriburl = contrib.url("https://cloud.r-project.org/"))[, 1],
                   bioc = {BiocManager::available()},
                   local = list.dirs(.libPaths()[1], full.names = FALSE, recursive = FALSE)
                   )
    attr(pkgs, "type") <- "ava"
    attr(pkgs, "repo") <- repo
    attr(pkgs, "new") <- FALSE
    save_cache(pkgs)
  }
  pkgs
}
