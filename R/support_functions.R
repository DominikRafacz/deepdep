# TODO: document this functions

DB <- function(api, head = 1e6, tail = head) {
  
  ret <- paste0("http://crandb.r-pkg.org", "/", api)
  ret <- httr::GET(ret)
  ret <- httr::content(ret, as = "text", encoding = "UTF-8")
  ret <- jsonlite::prettify(ret)
  ret <- skip_lines(ret, head = head, tail = tail)
  
  ret
}

skip_lines <- function(text, head = 1e6, tail = 1e6) {
  
  text <- strsplit(text, "\n")[[1]]
  
  tail <- min(tail, max(0, length(text) - head))
  
  skip_text <- if (length(text) > head + tail) {
    paste("\n... not showing", length(text) - head - tail, "lines ...\n")
  } else {
    character()
  }
  
  ret <- c(head(text, head), skip_text, tail(text, tail))
  ret <- paste(ret, collapse = "\n")
  ret
}

#' @importFrom utils available.packages contrib.url
#'
is_available <- function(package, bioc = FALSE) {
  package %in% get_available_packages(bioc)
}

check_package_name <- function(package, bioc) {
  if (!is_available(package, bioc)) {
    if (bioc) stop(paste0(package, " is not available neither on CRAN nor on Bioconductor"))
    else stop(paste0(package, " is not available on CRAN"))
  }
}

get_available_packages <- function(bioc = FALSE) {
  repositories <- "CRAN"
  if (bioc) repositories <- c(repositories, "bioc")
  pkgs <- NULL
  for (repo in repositories) {
    pkgs <- c(pkgs, get_available_packages_cached(repo))
  }
  pkgs
}

get_available_packages_cached <- function(repo = "CRAN") {
  check_repository(repo)
  cache_dir <- tempdir()
  ava_pkg_file <- paste0(cache_dir, "/deepdep_ava_", repo, "_pkg.RDS")
  pkgs <- NULL
  if (!file.exists(ava_pkg_file) || 
      is_too_old(ava_pkg_file)) {
    pkgs <- get_available_packages_repo(repo)
    saveRDS(pkgs, ava_pkg_file)
    pkgs
  } else {
    readRDS(ava_pkg_file)
  }
}

get_available_packages_repo <- function(repo = "CRAN") {
  switch (repo,
    CRAN = available.packages(contriburl = contrib.url("https://cloud.r-project.org/"))[, 1],
    bioc = BiocManager::available()
  )
}

check_repository <- function(repo) {
  if (repo == "CRAN") return()
  else if (repo == "bioc") check_bioc_installed()
  else stop("you can only search for packages on CRAN or 'Bioconductor' repositiories")
}

is_too_old <- function(file) {
   difftime(Sys.time(), file.info(file)$mtime, units = "secs") > 600
}

check_bioc_installed <- function() {
  if (!is_available("BiocManager")) 
    stop("You cannot use 'bioc' option if you don't have 'BiocManager' package installed")
}