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
is_available <- function(package) {
  package %in% get_available_packages()
}

check_package_name <- function(package) {
  if (!is_available(package)) stop(paste0(package, " is not on CRAN."))
}

get_available_packages <- function() {
  cache_dir <- tempdir()
  ava_pkg_file <- paste0(cache_dir, "/deepdep_ava_pkg.RDS")
  if (!file.exists(ava_pkg_file) || 
      difftime(Sys.time(), file.info(ava_pkg_file)$mtime, units = "secs") > 600) {
    ava_pkg <- available.packages(contriburl = contrib.url("https://cloud.r-project.org/"))[, 1]
    saveRDS(ava_pkg, ava_pkg_file)
    ava_pkg
  } else {
    readRDS(ava_pkg_file)
  }
}