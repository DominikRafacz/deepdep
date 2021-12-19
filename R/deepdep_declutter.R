deepdep_declutter <- function(dd) {
  depth <- max(dd[["dest_level"]])
  # Remove Suggests and Enhances from outer layers
  dd <- dd[dd[["origin"]] == attr(dd, "package_name") |
             dd[["type"]] %in% match_dependency_type("strong"), ]
  # Recompute package distances to the center
  G <- igraph::graph_from_data_frame(dd)
  pkg_distances <- igraph::distances(G, 1, mode = "out")
  # Remove dependencies of packages that are at least depth away from the center
  pkg_to_keep <- igraph::vertex_attr(G, "name")[pkg_distances < depth]
  dd <- dd[dd[["origin"]] %in% pkg_to_keep, ]
  # Update layer positioning
  dd[["origin_level"]] <- pkg_distances[, dd[["origin"]]]
  dd[["dest_level"]] <- pkg_distances[, dd[["name"]]]
  
  attr(dd, "decluttered") <- TRUE
  return(dd)
}
