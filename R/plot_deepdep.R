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
#' @param n_iter Number of iterations made to optimize layout.
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
#' @importFrom ggforce geom_circle
#' @importFrom igraph V
#' @importFrom igraph graph_from_data_frame
#' @importFrom graphlayouts draw_circle
#' @import ggraph
#' @export
plot.deepdep <- function(x, plot_type, same_level = FALSE, n_iter = 10, ...) {
  # stopifnot(is.deepdep(x)) # we need this function guys
  
  # we don't use pipes here, right?
  G <- graph_from_data_frame(x)
  G <- add_layers_to_vertices(G)
  if (!same_level) {
    G <- delete_edges_within_layer(G)
  }
  
  switch (plot_type,
    circular = {
      g <- ggraph(graph = G, layout = "focus", focus = 1) +
        draw_circle(use = "focus", max.circle = max(V(G)$layer - 1)) +
        geom_edge_link(aes(colour = type), arrow = arrow(angle = 16.6, ends = "first", type = "closed")) +
        geom_node_point(aes(fill = as.factor(layer)), size = 2, shape = 21) +
        geom_node_label(aes(label = names(V(G)), fill = factor(layer))) +
        coord_fixed() +
        theme_void()
    },
    tree = {
      g <- ggraph(G, "tree") +
        geom_edge_link(aes(colour = factor(type)), arrow = arrow(angle = 16.6, ends = "first", type = "closed")) +
        geom_node_point(aes(colour = factor(layer)), size = 5) +
        geom_node_label(aes(label = names(V(G)), fill = factor(layer))) +
        theme_void()
    }
  )
  g
}

# some private functions

#' @title Add layer property to graph vertices
#' 
#' @param G An \code{igraph} object
#' @import igraph 
add_layers_to_vertices <- function(G) {
  V(G)$layer <- distances(G, v = V(G)[1]) + 1
  G
}

#' @title Remove edges on the same level
#' 
#' @param G An \code{igraph} object
#' @import igraph 
delete_edges_within_layer <- function(G) {
  edges_to_delete <- E(G)[
    head_of(G, E(G))$layer == tail_of(G, E(G))$layer]
  delete_edges(G, edges_to_delete)
}
