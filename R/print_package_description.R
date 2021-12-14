#' @title Print function for an object of \code{package_description} class
#'
#' @param x An object of \code{package_description} class.
#' @param ... other
#'
#' @examples
#' \donttest{
#' library(deepdep)
#'
#' description <- get_description("ggplot2")
#' description
#' }
#'
#' @rdname print.package_description
#' @export
print.package_description <- function(x, ...) {
  # TODO: Maybe make use of default packageDescription class by collapsing
  #  dependency, maintainer and URL fields into comma-separated strings?
  cat(x$package, " (v", x$version, "): ", x$title, "\n", sep = "")
  cat("Maintainer:", x$maintainer, "\n")
  cat("Description: \n", x$description, "\n")
  cat("Depends:", names(x$depends), "\n")
  cat("Imports:", names(x$imports), "\n")
  cat("LinkingTo:", names(x$linkingto), "\n")
  cat("Suggests:", names(x$suggests), "\n")
  cat("Enhances:", names(x$enhances), "\n")
  cat("Scrap date:", x$crandb_file_date)
}
