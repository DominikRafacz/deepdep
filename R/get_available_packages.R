#' @export
get_available_packages <- function(bioc = FALSE, local = FALSE, reset_cache = FALSE) {
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
                   local = stop("NOT IMPLEMENTED YET"))
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