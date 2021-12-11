#' @title Scrap the DESCRIPTION file and CRAN metadata of the package
#'
#' @description This function uses api  of \href{http://crandb.r-pkg.org}{\bold{CRAN Data Base}}
#' to scrap the DESCRIPTION file and CRAN metadata of the package. It caches the results to speed the computation process.
#'
#' @param package A \code{character}. Name of the package that is on CRAN, Bioconductor repository or locally installed.
#' See \code{bioc} and \code{local} arguments.
#' @param bioc A \code{logical} value. If \code{TRUE} the Bioconductor dependencies data will be taken from the
#' Bioconductor repository. For this option to work properly, \code{BiocManager} package needs to be installed.
#' @param local A \code{logical} value. If \code{TRUE} only data of locally installed packages will be used (without API usage).
#' @param reset_cache A \code{logical} value. If \code{TRUE} the cache will be cleared before obtaining the list of packages.
#' @param version A \code{character}. Name of the version published on CRAN that should be retrieved.
#' Does not work for Bioconductor or local packages. Defaults to \code{NULL} which means 'get the latest version available'.
#'
#' @return An object of \code{package_description} class.
#'
#'
#' @examples
#' library(deepdep)
#'
#' \donttest{
#' description <- get_description("ggplot2")
#' description
#'
#' description_local <- get_description("deepdep", local = TRUE)
#' description_local
#' }
#'
#' @export
get_description <- function(package, bioc = FALSE, local = FALSE,
                            reset_cache = FALSE, version = NULL) {
  if (local && bioc) stop("You cannot use both 'local' and 'bioc' options at once.")
  if (local && !is.null(version)) stop("You cannot specify version for local package.")
  if (reset_cache) reset_cached_files("desc")
  if (!is_available(package, bioc, local)) return(NULL)
  if (local) return(get_desc_cached(package, "local"))
  desc <- NULL
  if (bioc) desc <- get_desc_cached(package, "bioc")
  if (is.null(desc)) desc <- get_desc_cached(package, "CRAN", version)
  desc
}

get_desc_cached <- function(package, repo, version = NULL) {
  descs <- get_cached_obj("desc", repo)
  if (package %in% names(descs))
    return(descs[[package]])
  descs <- switch(repo,
                  CRAN = update_descs_CRAN(package, descs, version),
                  bioc = update_descs_bioc(descs),
                  local = update_descs_local(package, descs))
  attr(descs, "new") <- FALSE
  save_cache(descs)
  descs[[package]]
}

#' @importFrom pkgsearch cran_package
update_descs_CRAN <- function(package, descs, version = NULL) {
  descs[[package]] <- cran_package(package, version = version) |>
    select_fields() |>
    remove_whitespace() |>
    replace_missing_dep_versions() |>
    split_URL() |>
    add_class_to_desc("CRAN")
  descs
}

update_descs_bioc <- function(descs) {
  # Whatever the line below means, caching could use some simplifying.
  # Actually, it doesn't even do anything, because there is no return.
  # Keeping it just as a reminder that there was some idea about this.
  
  # if file is not new, it means package is not available via bioc
  if (!attr(descs, "new")) descs
  
  # Actual working code now
  bioc_descs <- BiocPkgTools::biocPkgList() |>
    select_fields()
  for (index in seq_len(nrow(bioc_descs))) {
    descs[[bioc_descs[[index, "package"]]]] <- bioc_descs[index, ] |>
      as.list() |>
      remove_empty_dependencies() |>
      reformat_dependencies() |>
      remove_whitespace() |>
      paste_maintainer() |>
      split_URL() |>
      add_class_to_desc("Bioconductor")
  }
  
  attr(descs, "type") <- "desc"
  attr(descs, "repo") <- "bioc"
  descs
}

#' @importFrom utils packageDescription
update_descs_local <- function(package, descs) {
  descs[[package]] <- packageDescription(package) |>
    select_fields() |>
    remove_whitespace() |>
    split_dependencies() |>
    reformat_dependencies() |>
    paste_maintainer() |>
    split_URL() |>
    add_class_to_desc("local")
  descs
}

# Dependency related functions ----
filter_dep_types <- function(desc) {
  dep_types <- c("depends", "imports", "suggests", "enhances", "linkingto")
  dep_types[dep_types %in% names(desc)]
}

replace_missing_dep_versions <- function(desc) {
  dep_types <- filter_dep_types(desc)
  # If dependency version is not specified (marked by "*"), replace with NA
  desc[dep_types] <- lapply(desc[dep_types], \(dep_type) {
    lapply(dep_type, \(x) ifelse(x == "*", NA, x))
  })
  desc
}

split_dependencies <- function(desc) {
  dep_types <- filter_dep_types(desc)
  desc[dep_types] <- lapply(desc[dep_types], strsplit, ",")
  desc
}

remove_empty_dependencies <- function(desc) {
  dep_types <- c("depends", "imports", "suggests", "enhances", "linkingto")
  is_empty_dep_type <- vapply(desc[dep_types], \(x) all(is.na(x[[1]])), logical(1))
  desc[dep_types[is_empty_dep_type]] <- NULL
  desc
}

reformat_dependencies <- function(desc) {
  dep_types <- filter_dep_types(desc)
  # Separate and reformat names and versions of packages
  desc[dep_types] <- lapply(desc[dep_types], \(dep_type) {
    search_res <- lapply(dep_type[[1]], \(x) {
      stringi::stri_match_all_regex(x, "([^(]*)(?:\\((.*)\\))?")[[1]][-2, ]
    })
    setNames(lapply(search_res, `[`, 3), lapply(search_res, `[`, 2))
  })
  desc
}

# Other processing functions ----
select_fields <- function(desc) {
  fields <- c("package", "title", "maintainer", "description", "url", "license",
              "depends", "imports", "suggests", "linkingto", "enhances", "crandb_file_date")
  # TODO: use date/publication instead of crandb_file_date to include Bioconductor and local?
  names(desc) <- tolower(names(desc))
  desc[fields[fields %in% names(desc)]]
}

remove_whitespace <- function(desc) {
  rapply(desc, \(x) gsub("\n", " ", x, fixed = TRUE), how = "list")
}

split_URL <- function(desc) {
  # Extract a vector of URLs from comma-split text
  if (!is.null(desc[["url"]])) {
    desc[["url"]] <- desc[["url"]] |>
      strsplit(",") |>
      unlist() |>
      trimws()
  }
  desc
}

paste_maintainer <- function(desc) {
  # Combine a vector of strings into one comma-separated string
  if (!is.null(desc[["maintainer"]])) {
    desc[["maintainer"]] <- paste0(desc[["maintainer"]], collapse = ", ")
  }
  desc
}

add_class_to_desc <- function(desc, source) {
  attr(desc, "package_name") <- desc[["package"]]
  attr(desc, "source") <- source
  class(desc) <- c("package_description", "list")
  desc
}
