#' @title Print function for an object of \code{package_description} class
#'
#' @param x An object of \code{package_description} class.
#' @param ... other
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
