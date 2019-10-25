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
get_description <- function(package) {
  
  if (!is_available(package)) return(NULL)
  
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
  if (!is.null(description$depends)) {
    description$depends <- lapply(description$depends, function(x) ifelse(x == "*", NA, x))
  }
  
  if (!is.null(description$imports)) {
    description$imports <- lapply(description$imports, function(x) ifelse(x == "*", NA, x))
  }
  
  if (!is.null(description$suggests)) {
    description$suggests <- lapply(description$suggests, function(x) ifelse(x == "*", NA, x))
  }
  
  if (!is.null(description$enhances)) {
    description$enhances <- lapply(description$enhances, function(x) ifelse(x == "*", NA, x))
  }
  
  if (!is.null(description$linkingto)) {
    description$linkingto <- lapply(description$linkingtos, function(x) ifelse(x == "*", NA, x))
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
  ret
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
