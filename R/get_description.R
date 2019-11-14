#' @title Scrap the DESCRIPTION file and CRAN metadata of the package
#'
#' @description This function uses \href{https://github.com/r-hub/crandb}{\bold{API}} of
#' \href{http://crandb.r-pkg.org}{\bold{CRAN Data Base}} to scrap the DESCRIPTION file and
#' CRAN metadata of the package. It caches the results to speed the computation process.
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
#' description <- get_description("ggplot2")
#' description
#'
#' \dontrun{
#' description_local <- get_description("deepdep", local = TRUE)
#' description_local
#' }
#'
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
                  CRAN = append_desc_CRAN(package, descs),
                  bioc = get_all_desc_bioc(descs),
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

check_if_valid_depend <- function(char) {
  grepl("Depends", char, fixed = TRUE) |
    grepl("Imports", char, fixed = TRUE) |
    grepl("Suggests", char, fixed = TRUE) |
    grepl("Enhances", char, fixed = TRUE) |
    grepl("LinkingTo", char, fixed = TRUE)
}
