#' @title Scrap the DESCRIPTION file and CRAN metadata of the package
#' 
#' @description  [API](https://github.com/r-hub/crandb) [CRAN Data Base](http://crandb.r-pkg.org)
#' 
#' @param package A \code{character}. Name of the package that is on CRAN.
#'
#' @return An object of \code{package_description} class. 
#' 
#' @author Hubert Baniecki, Szymon Maksymiuk
#' 
#' @examples
#' library(deepdep)
#' 
#' desc <- get_description("stringr")
#' desc
#' 
#'
#' @export
get_description <- function(package, bioc = FALSE, local = FALSE, reset_cache = FALSE) {
  if (reset_cache) reset_cached_files("desc")
  if (!is_available(package, bioc, local)) return(NULL)
  if (local) return(get_desc_cached(package, "local"))
  desc <- NULL
  if (bioc) desc <- get_desc_cached(package, "bioc")
  if (is.null(desc)) desc <- get_desc_cached(package, "CRAN")
  desc
}

#' @title Print function for an object of \code{package_description} class
#' 
#' @param x An object of \code{package_description} class.
#' @param ... other
#'
#' @author Hubert Baniecki, Szymon Maksymiuk
#' 
#' @examples
#' library(deepdep)
#' 
#' desc <- get_description("stringr")
#' desc
#' 
#' @rdname print.package_description
#' @export
print.package_description <- function(x, ...) {
  cat(x$package, ": ", x$title, "\n", sep = "")
  cat("Maintainer:", x$maintainer, "\n")
  cat("Description: \n", x$description, "\n")
  cat("Depends:", names(x$depends), "\n")
  cat("Imports:", names(x$imports), "\n")
  cat("LinkingTo:", names(x$linkingto), "\n")
  cat("Suggests:", names(x$suggests), "\n")
  cat("Enhances:", names(x$enhances), "\n")
  cat("Scrap date:", x$crandb_file_date)
}

get_desc_cached <- function(package, repo) {
  descs <- get_cached_obj("desc", repo)
  if (package %in% names(descs))
    return(descs[[package]])
  descs <- switch(repo,
                  CRAN = append_desc_CRAN(package, descs),
                  bioc = get_all_desc_bioc(),
                  local = get_desc_local(package, descs))
  attr(descs, "new") <- FALSE
  save_cache(descs)
  descs[[package]]
}

append_desc_CRAN <- function(package, descs) {
  # get the description
  json_as_string <- DB(package)
  description <- jsonlite::fromJSON(json_as_string)
  
  # prettify the description
  names(description) <- tolower(names(description))
  
  # authors is a vector of "person" class objects (named character)
  description$author <- NULL
  names(description)[names(description) == "authors@r"] <- "authors"
  
  # gsub("\n") did not remove all backslashes
  # gsub("\\") dit no work
  # this apparently works with '\n' in input 
  # this below was very optimistic
  # description$authors <- eval(parse(text = description$authors))
  # this below was very optimistic too
  # description$description <- gsub("\n", "", x = description$description, fixed = TRUE)
  
  # add NA if a version of the dependency is not specified (instead of "*")
  for (dep_type in c("depends", "imports", "suggests", "enhances", "linkingto")) {
    if (!is.null(description[[dep_type]]))
      description[[dep_type]] <- 
        lapply(description[[dep_type]], function(x) ifelse(x == "*", NA, x))
  }
  
  # change url to the vector of properl urls
  if (!is.null(description$url)) {
    description$url <- gsub("\n", "", x = description$url, fixed = TRUE) 
    description$url <- unlist(strsplit(description$url, ","))
  }
  
  names(description)[names(description) == "date/publication"] <- "publication_date"
  
  # what is date?
  description$date <- NULL
  
  # what is releases?
  description$releases <- NULL

  ret <- description
  
  attr(ret, "package_name") <- package
  class(ret) <- c("package_description", "list")
  descs[[package]] <- ret
  descs
}

#' @importFrom httr GET content
#' @importFrom stringi stri_match_all_regex stri_replace_all_regex
get_all_desc_bioc <- function(descs) {
  # if file is not new, it means package is not available via bioc
  if (!attr(descs, "new")) descs
  
  # get all descriptions from bioconductor repository
  tmp <- GET("http://bioconductor.org/packages/release/bioc/VIEWS")
  tmp <- content(tmp, as = "text", encoding = "UTF-8")
  
  # parse the text - split everything into pairs using format "key: value"
  mat <- stri_match_all_regex(tmp, "(.*):(?> |\\n)((?>.|\\n        )*)\\n")[[1]][, -1]
  n <- nrow(mat)
  mat[,2] <- stri_replace_all_regex(mat[, 2], "(\\n)?        |\\n", " ")
  pkg_begs <- (1:n)[mat[, 1] == "Package"]
  pkg_ends <- c((pkg_inds - 1)[-1], n)
  
  # transform two-column matrix into a list
  lapply(1:length(pkg_inds), function(i) {
    ret <- as.list(mat[pkg_inds[i]:pkg_ends[i], 2])
    names(ret) <- mat[pkg_inds[i]:pkg_ends[i], 1]
    ret
  }) -> pkgs
  
  # name pakcages
  names(pkgs) <- lapply(pkgs, function(pkg) pkg$Package)
  
  # convert strings of dependencies into vectors
  descs <- lapply(pkgs, function(pkg) {
    nms <- tolower(names(pkg))
    names(pkg) <- nms
    for (dep_type in c("depends", "imports", "suggests", "linkingto", "enhances")) {
      if (dep_type %in% nms) {
        deps <- stri_match_all_regex(
          pkg[[dep_type]], 
          "(?>\\s*)([^,\\(]+)(?>(?> \\()(\\>\\=[^)]+)(?>\\)))?(?>,|$)")[[1]]
        pkg[[dep_type]] <- deps[,3]
        names(pkg[[dep_type]]) <- deps[,2]
      }
    }
    pkg
  })

  attr(descs, "type") <- "desc"
  attr(descs, "repo") <- "bioc"
  descs
}

get_desc_local <- function(package, descs) {
  #pacakge - string giving pacakge name
  #descs - list of already scrapped descriptions
  stop("NOT IMPLEMENTED YET")
  #this function should append descs by description of pacakge 
}