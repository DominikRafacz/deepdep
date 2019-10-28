#' @title Plot dependency data
#' 
#' @description Visualize dependency data passed as \code{deepdep} object using
#' \code{ggplot2} and \code{ggraph} packages. Several tree-like layouts are
#' available
#' 
#' @param x A \code{\link{deepdep}} object.
#' @param plot_type A \code{character}. Possible values are \code{circular} and \code{tree}.
#' @param same_level A \code{boolean}. Whether to plot links between dependencies on the same
#' level.
#' @param ... Other arguments passed to plotting function.
#' 
#' @author Mateusz Bąkała
#' 
#' @examples
#' library(deepdep)
#' 
#' dd <- deepdep("stringr")
#' plot(dd, "tree")
#' 
#' dd2 <- deepdep("cranlogs", depth = 2)
#' plot(dd2, "circular")
#' 
#' @export
plot.deepdep <- function(x, plot_type, same_level = FALSE, ...) {
  # stopifnot(is.deepdep(x)) # we need this function guys
  
  # we don't use pipes here, right?
  G <- igraph::graph_from_data_frame(x)
  G <- add_layers_to_vertices(G)
  if (!same_level) {
    G <- delete_edges_within_layer(G)
  }
  
  switch (plot_type,
    circular = {
      g <- ggraph::ggraph(G, "tree", circular = TRUE) +
        ggforce::geom_circle(ggplot2::aes(x0 = 0, y0 = 0, r = (layer-1)/max(layer-1), colour = factor(layer)), size = 3) +
        ggraph::geom_edge_link(ggplot2::aes(colour = factor(type)), arrow = grid::arrow(angle = 16.6, ends = "first", type = "closed")) +
        ggraph::geom_node_point(ggplot2::aes(colour = factor(layer)), size = 5) +
        ggraph::geom_node_label(ggplot2::aes(label = names(igraph::V(G)), fill = factor(layer))) +
        ggplot2::xlim(c(-1, 1)) +
        ggplot2::ylim(c(-1, 1)) +
        ggplot2::coord_equal() +
        ggplot2::theme_void()
    },
    tree = {
      g <- ggraph::ggraph(G, "tree") +
        ggraph::geom_edge_link(ggplot2::aes(colour = factor(type)), arrow = grid::arrow(angle = 16.6, ends = "first", type = "closed")) +
        ggraph::geom_node_point(ggplot2::aes(colour = factor(layer)), size = 5) +
        ggraph::geom_node_label(ggplot2::aes(label = names(igraph::V(G)), fill = factor(layer))) +
        ggplot2::theme_void()
    }
  )
  g
}

# some private functions

#' @title Add layer property to graph vertices
#' 
#' @param G An \code{igraph} object
add_layers_to_vertices <- function(G) {
  igraph::V(G)$layer <- igraph::distances(G, v = igraph::V(G)[1]) + 1
  G
}

#' @title Remove edges on the same level
#' 
#' @param G An \code{igraph} object
delete_edges_within_layer <- function(G) {
  edges_to_delete <- igraph::E(G)[
    igraph::head_of(G, igraph::E(G))$layer == igraph::tail_of(G, igraph::E(G))$layer]
  igraph::delete_edges(G, edges_to_delete)
}
