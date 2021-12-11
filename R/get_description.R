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
get_description <- function(package, bioc = FALSE, local = FALSE, reset_cache = FALSE) {
  if (local && bioc) stop("You cannot use both 'local' and 'bioc' options at once.")
  if (reset_cache) reset_cached_files("desc")
  if (!is_available(package, bioc, local)) return(NULL)
  if (local) return(get_desc_cached(package, "local"))
  desc <- NULL
  if (bioc) desc <- get_desc_cached(package, "bioc")
  if (is.null(desc)) desc <- get_desc_cached(package, "CRAN")
  desc
}

get_desc_cached <- function(package, repo) {
  descs <- get_cached_obj("desc", repo)
  if (package %in% names(descs))
    return(descs[[package]])
  descs <- switch(repo,
                  CRAN = update_descs_CRAN(package, descs),
                  bioc = get_all_desc_bioc(descs),
                  local = get_desc_local(package, descs))
  attr(descs, "new") <- FALSE
  save_cache(descs)
  descs[[package]]
}

#' @importFrom pkgsearch cran_package
update_descs_CRAN <- function(package, descs) {
  descs[[package]] <- cran_package(package, version = NULL) |>
    select_fields() |>
    remove_whitespace() |>
    replace_missing_dep_versions() |>
    split_URL() |>
    add_class_to_desc("CRAN")
  descs
}

update_descs_bioc <- function(descs) {
  bioc_descs <- BiocPkgTools::biocPkgList() |>
    select_fields()
  for (index in seq_len(nrow(bioc_descs))) {
    descs[[bioc_descs[[index, "package"]]]] <- bioc_descs[index, ] |>
      as.list() |>
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

select_fields <- function(desc) {
  fields <- c("package", "title", "maintainer", "description", "url", "license",
              "depends", "imports", "suggests", "linkingto", "enhances", "crandb_file_date")
  # TODO: use date/publication instead of crandb_file_date to include Bioconductor?
  names(desc) <- tolower(names(desc))
  desc[fields[fields %in% names(desc)]]
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

# 
add_class_to_desc <- function(desc, source) {
  attr(desc, "package_name") <- desc[["package"]]
  attr(desc, "source") <- source
  class(desc) <- c("package_description", "list")
  desc
}

#' @importFrom httr GET content
get_all_desc_bioc <- function(descs) {
  # if file is not new, it means package is not available via bioc
  if (!attr(descs, "new")) descs

  # get all descriptions from bioconductor repository
  tmp <- GET("http://bioconductor.org/packages/release/bioc/VIEWS")
  tmp <- content(tmp, as = "text", encoding = "UTF-8")

  pkgs <- prepeare_descs(tmp)
  # name pakcages
  names(pkgs) <- lapply(pkgs, function(pkg) pkg$Package)

  # convert strings of dependencies into vectors
  descs <- lapply(pkgs, ajust_desc_file)

  attr(descs, "type") <- "desc"
  attr(descs, "repo") <- "bioc"
  descs
}

prepeare_descs <- function(raw_desc) {
  mat <- stringi::stri_match_all_regex(raw_desc, "(.*):(?> |\\n)((?>.|\\n        )*)\\n")[[1]][, -1]
  n <- nrow(mat)
  mat[,2] <- stringi::stri_replace_all_regex(mat[, 2], "(\\n)?        |\\n", " ")
  pkg_begs <- (1:n)[mat[, 1] == "Package"]
  pkg_ends <- c((pkg_begs - 1)[-1], n)

  # transform two-column matrix into a list
  lapply(1:length(pkg_begs), function(i) {
    ret <- as.list(mat[pkg_begs[i]:pkg_ends[i], 2])
    names(ret) <- mat[pkg_begs[i]:pkg_ends[i], 1]
    ret
  }) -> pkgs
  pkgs
}

ajust_desc_file <- function(pkg) {
  nms <- tolower(names(pkg))
  names(pkg) <- nms
  for (dep_type in c("depends", "imports", "suggests", "linkingto", "enhances")) {
    if (dep_type %in% nms) {
      deps <- stringi::stri_match_all_regex(
        pkg[[dep_type]],
        "(?>\\s*)([^,\\(]+)(?>(?> \\()(\\>\\=[^)]+)(?>\\)))?(?>,|$)")[[1]]
      pkg[[dep_type]] <- deps[,3]
      names(pkg[[dep_type]]) <- deps[,2]
    }
  }
  attr(pkg, "package_name") <- pkg$Package
  class(pkg) <- c("package_description", "list")
  pkg
}

get_desc_local <- function(package, descs) {
  # get path to DESCRIPTION file of the package
  path <- paste(.libPaths()[1], package, "DESCRIPTION", sep = "/")
  # get the description
  raw_desc <- readLines(path)

  # prepeare DESCRIPTION so same function as for bioconductor can be used. Packages has to be listed in one line.
  merge <- list()
  counter <- 0
  for (i in 1:length(raw_desc)) {
    if (check_if_valid_depend(raw_desc[i])) {
      start <- i
    }
    if (counter > 0) {
      if (!grepl(":", raw_desc[i], fixed = TRUE)) {
        counter <- counter + 1
        if (grepl(":", raw_desc[i + 1], fixed = TRUE)) {
          merge[[i]] <- c(start, counter)
          counter <- 0
        }
      }
    }
  }

  for (vec in merge) {
    if(is.null(vec)) next()
    raw_desc[vec[1]] <-  paste0(raw_desc[vec[1]:(vec[1]+vec[2]-1)], collapse = " ")
    raw_desc[(vec[1]+1):(vec[1] + vec[2]-1)] <- ""
  }

  raw_desc <- paste0(raw_desc, collapse = "\n")

  # Change raw desc to list
  pkg <- prepeare_descs(raw_desc)[[1]]

  ret <- ajust_desc_file(pkg)

  attr(ret, "package_name") <- package
  class(ret) <- c("package_description", "list")
  descs[[package]] <- ret
  descs
}

# # Attach input list for easier references
# attach(desc)
# list(
#   package = Package,
#   title = Title,
#   maintainer = Maintainer,
#   description = Description,
#   depends = Depends,
#   crandb_file_date = crandb_file_date
# )

check_if_valid_depend <- function(char) {
  grepl("Depends", char, fixed = TRUE) |
    grepl("Imports", char, fixed = TRUE) |
    grepl("Suggests", char, fixed = TRUE) |
    grepl("Enhances", char, fixed = TRUE) |
    grepl("LinkingTo", char, fixed = TRUE)
}
