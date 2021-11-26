#' @title Acquire the dependencies of the package
#'
#' @description This function uses \code{\link{get_description}} and \code{\link{get_downloads}}
#' to acquire the dependencies of the package (with their downloads).
#'
#' @param package A \code{character}. Name of the package that is on CRAN, Bioconductor repository or locally installed.
#' See \code{bioc} and \code{local} arguments.
#' @param downloads A \code{logical}. If \code{TRUE} add package downloads data. By default it's \code{TRUE}.
#' @param bioc A \code{logical} value. If \code{TRUE} the Bioconductor dependencies data will be taken from the
#' Bioconductor repository. For this option to work properly, \code{BiocManager} package needs to be installed.
#' @param local A \code{logical} value. If \code{TRUE} only data of locally installed packages will be used (without API usage).
#' @param dependency_type A \code{character} vector. Types of the dependencies that should be sought, a subset of
#' \code{c("Imports", "Depends", "LinkingTo", "Suggests", "Enhances")}. Other possibilities are: character string
#' \code{"all"}, a shorthand for that vector; character string \code{"most"} for the same vector without \code{"Enhances"};
#' character string \code{"strong"} (default) for the first three elements of that vector. Works analogously to
#' \code{\link{tools::package_dependencies}}.
#'
#' @return An object of \code{package_dependencies} class.
#'
#' @seealso \code{\link{get_description}} \code{\link{get_downloads}}
#'
#' @examples
#' library(deepdep)
#'
#' \donttest{
#' dependencies <- get_dependencies("htmltools", downloads = FALSE)
#' dependencies
#'
#' dependencies_local <- get_dependencies("deepdep", downloads = FALSE, local = TRUE)
#' dependencies_local
#' }
#'
#' @export
get_dependencies <- function(package, downloads = TRUE, bioc = FALSE, local = FALSE,
                             dependency_type = "strong") {

  if (downloads && (local || bioc)) stop("If you use downloads, you cannot use",
                                         " neither bioc nor local")

  dependency_type <- match_dependency_type(dependency_type)

  l_dependency_type <- tolower(dependency_type)
  names(dependency_type) <- l_dependency_type

  description <- get_description(package, bioc, local)

  deps <- description[l_dependency_type]

  package_names <- unlist(sapply(deps, names), use.names = FALSE)
  package_versions <- unlist(deps, use.names = FALSE)
  package_types <- unlist(sapply(names(deps), function(dep_type)
    rep(dependency_type[dep_type], length(deps[[dep_type]]))), use.names = FALSE)

  if (!is.null(package_names) && length(package_names) > 0) {
    downloads_df <- NULL
    remove_base_or_R <- sapply(package_names,
                               function(pkg_name) is_available(pkg_name, bioc, local))

    if (downloads) {
      downloads_list <- lapply(package_names, get_downloads)
      downloads_df <- as.data.frame(do.call(rbind, downloads_list))
    }

    # this works if downloads_df is NULL
    ret <- as.data.frame(cbind(name = package_names[remove_base_or_R],
                               version = package_versions[remove_base_or_R],
                               type = package_types[remove_base_or_R],
                               downloads_df),
                         stringsAsFactors = FALSE)
    # deletes downloads_df column if ret is empty
    if (is.null(downloads_df)) {
      ret$downloads_df <- downloads_df
    }
  } else return(NULL)

  attr(ret, "package_name") <- package
  row.names(ret) <- NULL
  class(ret) <- c("package_dependencies", "data.frame")
  ret
}

#' @title Print function for an object of \code{package_dependencies} class
#'
#' @param x An object of \code{package_dependencies} class.
#' @param ... other
#'
#'
#' @examples
#' \donttest{
#' library(deepdep)
#'
#' get_dependencies("htmltools", downloads = TRUE)
#' }
#'
#' @rdname print.package_dependencies
#' @export
print.package_dependencies <- function(x, ...) {
  print.data.frame(x)
}

#' Match vector of dependency types
#'
#' @inheritParams get_dependencies
#'
#' Based on `tools:::.expand_dependency_type_spec`, according to the suggestion of Dirk Eddelbuettel
#'
#' @noRd
match_dependency_type <- function(dependency_type) {
  possible_types <- c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")

  if (identical(dependency_type, "strong")) possible_types[1:3]
  else if (identical(dependency_type, "most")) possible_types[1:4]
  else if (identical(dependency_type, "all")) possible_types
  else if (!all(dependency_type %in% possible_types) || length(dependency_type) < 1)
    stop("'dependency_type' should specify which types of dependencies should be included")
  else dependency_type
}
