#' @title Main plot function for a \code{deepdep} object
#'
#' @description Visualize dependency data from a \code{deepdep} object using
#' \code{ggplot2} and \code{ggraph} packages. Several tree-like layouts are available.
#'
#' @param x A \code{\link{deepdep}} object or a \code{character} package name.
#' @param type A \code{character}. Possible values are \code{circular} and \code{tree}.
#' @param same_level A \code{logical}. If \code{TRUE} links between dependencies on the same
#' level will be added. By default it's \code{FALSE}.
#' @param label_percentage A \code{numeric} value between \code{0} and \code{1}. A fraction
#' of labels to be displayed. By default it's \code{1} (all labels displayed).
#' @param ... Other arguments passed to the plotting function.
#'
#' @return A \code{ggplot2, gg, ggraph, deepdep_plot} class object.
#'
#' @examples
#' library(deepdep)
#'
#' dd <- deepdep("stringr")
#' plot_dependencies(dd, "tree")
#'
#' dd2 <- deepdep("cranlogs", depth = 2)
#' plot_dependencies(dd2, "circular")
#'
#' plot_dependencies("mice", label_percentage = 0.3, downloads = TRUE)
#'
#' @importFrom ggforce geom_circle
#' @importFrom graphlayouts draw_circle
#' @importFrom stats quantile
#' @import ggplot2
#' @import ggraph
#' @import igraph
#'
#' @rdname plot_deepdep
#' @export
plot_dependencies <- function(x, type = "circular", same_level = FALSE,
                              label_percentage = 1, ...) {
  UseMethod("plot_dependencies")
}

#' @rdname plot_deepdep
#' @export
plot_dependencies.default <- function(x, type = "circular", same_level = FALSE,
                                      label_percentage = 1, ...) {
  stop("This type of object does not have implemented method for 'plot_dependencies'")
}

#' @rdname plot_deepdep
#' @export
plot_dependencies.character <- function(x, type = "circular", same_level = FALSE,
                                      label_percentage = 1, ...) {
  dd <- deepdep(x, ...)
  plot_dependencies(dd, type, same_level, label_percentage, ...)
}

#' @rdname plot_deepdep
#' @export
plot_dependencies.deepdep <- function(x, type = "circular", same_level = FALSE,
                                      label_percentage = 1, ...) {
  # Due to NSE inside of the function, we have to decleare "labeled" as NULL to prevent check fail
  labeled <- NULL

  G <- graph_from_data_frame(x)
  G <- add_layers_to_vertices(G)
  if (!same_level) {
    G <- delete_edges_within_layer(G)
  }

  # mark vertices to label
  pkg_downloads <- unlist(x[["grand_total"]])
  # central node should always be labeled
  V(G)$labeled <- c(TRUE, pkg_downloads >= quantile(pkg_downloads, probs = 1 - label_percentage))

  switch (type,
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

  class(g) <- c(class(g), "deepdep_plot")

  g
}

#' @title Add layer property to graph vertices
#' @noRd
#'
#' @param G An \code{igraph} object.
add_layers_to_vertices <- function(G) {
  V(G)$layer <- distances(G, v = V(G)[1]) + 1
  G
}

#' @title Remove edges on the same level
#' @noRd
#'
#' @param G An \code{igraph} object.
delete_edges_within_layer <- function(G) {
  edges_to_delete <- E(G)[
    head_of(G, E(G))$layer == tail_of(G, E(G))$layer]
  delete_edges(G, edges_to_delete)
}
