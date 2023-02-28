#' @importFrom stats setNames
#' @importFrom woodendesc filter_dependencies squash wood_dependencies
#' @export
deepdep <- function(package, depth = 1, type = "strong", version = NULL,
                    repos = c("core", "cran"),
                    # Backwards compatible parameters (use NULL or missing)
                    downloads = FALSE, bioc = FALSE, local = FALSE,
                    dependency_type = "strong") {
  # TODO: implement handling version code
  
  # TODO: implement backwards compatibility
  
  # TODO: test argument values
  package_levels <- setNames(0, package)
  level <- 0
  missing_pkgs <- character()
  
  deps <- data.frame(
    origin = character(),
    package = character(),
    version = character(),
    type = character(),
    origin_level = integer(),
    target_level = integer()
  )
  
  while (level < depth) {
    pkgs_to_query <- names(package_levels)[package_levels == level]
    # Remove R as a dependency
    pkgs_to_query <- pkgs_to_query[pkgs_to_query != "R"]
    
    new_deps <- wood_dependencies(pkgs_to_query, repos = repos)
    missing_pkgs <- update_missing(missing_pkgs, new_deps)
    new_deps <- filter_dependencies(new_deps, type)
    new_deps <- squash(new_deps)
    
    if (nrow(new_deps) == 0) break
    
    package_levels <- update_package_levels(package_levels, level, new_deps)
    new_deps <- add_levels(new_deps, package_levels, level)
    
    deps <- rbind(deps, new_deps)
    level <- level + 1
  }
  
  if (length(missing_pkgs) > 0) {
    warning(
      "Dependencies not found for: ",
      paste0(missing_pkgs, collapse = ", ")
    )
  }
  
  structure(
    deps,
    package = package,
    version = version,
    class = c("deepdep", "data.frame")
  )
}

update_missing <- function(missing_pkgs, deps) {
  c(missing_pkgs, names(deps)[vapply(deps, is.null, logical(1))])
}

update_package_levels <- function(package_levels, level, deps) {
  new_pkgs <- setdiff(deps[["package"]], names(package_levels))
  new_pkgs <- setNames(rep(level + 1, length(new_pkgs)), new_pkgs)
  c(package_levels, new_pkgs)
}

add_levels <- function(deps, package_levels, level) {
  deps[["origin_level"]] <- level
  deps[["target_level"]] <- package_levels[deps[["package"]]]
  deps
}
