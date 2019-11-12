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
#' @param label_percentage An \code{integer} or \code{boolean}. Should all labels be displayed
#' or only a fraction? Uses boolean casting to allow boolean use. Defaults to 1 (all labels
#' displayed).
#' @param ... Other arguments passed to plotting function.
#'
#' @examples
#' library(deepdep)
#'
#' dd <- deepdep("stringr")
#' plot_dependencies(dd, "tree")
#'
#' dd2 <- deepdep("cranlogs", depth = 2)
#' plot_dependencies(dd2, "circular")
#' @rdname plot_deepdep
#' @export
plot_dependencies <- function(x, plot_type = "circular", same_level = FALSE,
                              label_percentage = 1, ...) {
  UseMethod("plot_dependencies")
}

#' @rdname plot_deepdep
#' @exportMethod plot_dependencies default
#' @export
plot_dependencies.default <- function(x, plot_type = "circular", same_level = FALSE,
                                      label_percentage = 1, ...) {
  stop("This type of object does not have implemented method for 'plot_dependencies'")
}

#' @rdname plot_deepdep
#' @importFrom ggforce geom_circle
#' @importFrom igraph V
#' @importFrom igraph graph_from_data_frame
#' @importFrom graphlayouts draw_circle
#' @import ggplot2
#' @import ggraph
#' @exportMethod plot_dependencies deepdep
#' @export
plot_dependencies.deepdep <- function(x, plot_type = "circular", same_level = FALSE,
                                      label_percentage = 1, ...) {
  # Due to NSE inside of the function, we have to decleare "to" and "from" as NULL to prevent check fail
  type <- NULL

  G <- graph_from_data_frame(x)
  G <- add_layers_to_vertices(G)
  if (!same_level) {
    G <- delete_edges_within_layer(G)
  }

  # mark vertices to label
  pkg_downloads <- unlist(x[["grand_total"]])
  # central node should always be labeled
  V(G)$labeled <- c(TRUE, pkg_downloads >= quantile(pkg_downloads, probs = 1 - label_percentage))

  switch (plot_type,
    circular = {
      g <- ggraph(graph = G, layout = "focus", focus = 1) +
        draw_circle(use = "focus", max.circle = max(V(G)$layer - 1)) +
        geom_edge_link(aes_string(colour = "type"), arrow = arrow(angle = 16.6, ends = "first", type = "closed")) +
        geom_node_point(aes(fill = as.factor(layer)), size = 2, shape = 21, show.legend = FALSE) +
        geom_node_label(aes(label = ifelse(labeled, names(V(G)), ""), fill = factor(layer)),
                        show.legend = FALSE, repel = TRUE) +
        coord_fixed() +
        theme_void()
    },
    tree = {
      g <- ggraph(G, "tree") +
        geom_edge_link(aes_string(colour = factor("type")), arrow = arrow(angle = 16.6, ends = "first", type = "closed")) +
        geom_node_point(aes_string(colour = factor("layer")), size = 5, show.legend = FALSE) +
        geom_node_label(aes(label = ifelse(labeled, names(V(G)), ""), fill = factor(layer)),
                        show.legend = FALSE, repel = TRUE) +
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
