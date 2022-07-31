#' @importFrom stats setNames
#' @importFrom woodendesc wood_dependencies
#' @export
deepdep <- function(package, depth = 1, type = "strong", version = NULL,
                    repos = "cran") {
  # TODO: replace default repos with c("cran", "core")
  #  or use omit_core_packages = TRUE/FALSE that may append "core" to repos
  
  # TODO: test argument values
  type <- match_dependency_type(type)
  
  package_levels <- setNames(0, package)
  level <- 0
  missing_pkgs <- character()
  
  deps <- data.frame(
    package = character(),
    dependency = character(),
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
    if (is_empty(new_deps)) break
    
    new_deps <- transform_dependencies(new_deps)
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
    package_name = package,
    class = c("deepdep", "data.frame")
  )
}

update_missing <- function(missing_pkgs, deps) {
  c(missing_pkgs, names(deps)[vapply(deps, is.null, logical(1))])
}

filter_dependencies <- function(deps, type) {
  # Remove NULL dependencies
  deps <- deps[!vapply(deps, is.null, logical(1))]
  # Filter out dependencies not in type
  deps <- lapply(deps, function(dep) {
    dep[dep[["type"]] %in% type, ]
  })
  # Remove empty dependencies
  deps[vapply(deps, function(dep) nrow(dep) > 0, logical(1))]
}

transform_dependencies <- function(deps) {
  # Add origin column
  deps <- mapply(function(dep, name) {
    data.frame(origin = name, dep)
  }, deps, names(deps), SIMPLIFY = FALSE)
  # Merge data frames
  deps <- do.call(rbind, c(deps, make.row.names = FALSE))
  # Rename columns
  names(deps)[c(1, 2)] <- c("package", "dependency")
  deps
}

update_package_levels <- function(package_levels, level, deps) {
  new_pkgs <- setdiff(deps[["dependency"]], names(package_levels))
  new_pkgs <- setNames(rep(level + 1, length(new_pkgs)), new_pkgs)
  c(package_levels, new_pkgs)
}

add_levels <- function(deps, package_levels, level) {
  deps[["origin_level"]] <- level
  deps[["target_level"]] <- package_levels[deps[["dependency"]]]
  deps
}

# TODO: implement as a method for wood_deps/wood_dep_list maybe?
is_empty <- function(deps) {
  all(vapply(deps, nrow, numeric(1)) == 0)
}
