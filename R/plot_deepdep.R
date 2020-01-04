#' @title Main plot function for a \code{deepdep} object
#'
#' @description Visualize dependency data from a \code{deepdep} object using
#' \code{ggplot2} and \code{ggraph} packages. Several tree-like layouts are available.
#'
#' @param x A \code{\link{deepdep}} object or a \code{character} package name.
#' @param type A \code{character}. Possible values are \code{circular} and \code{tree}.
#' @param same_level A \code{logical}. If \code{TRUE} links between dependencies on the same
#' level will be added. By default it's \code{FALSE}.
#' @param reverse A \code{logical}. If \code{TRUE} links between dependencies pointing from
#' deeper level to more shallow level will be added. By default it's \code{FALSE}.
#' @param label_percentage A \code{numeric} value between \code{0} and \code{1}. A fraction
#' of labels to be displayed. By default it's \code{1} (all labels displayed).
#' @param version A \code{logical}. If \code{TRUE} required version of package will be
#' displayed below package name. Defaults to \code{FALSE}.
#' @param ... Other arguments passed to the \code{deepdep} function.
#'
#' @return A \code{ggplot2, gg, ggraph, deepdep_plot} class object.
#'
#' @examples
#' library(deepdep)
#'
#' dd <- deepdep("ggplot2")
#' plot_dependencies(dd, "tree")
#'
#' dd2 <- deepdep("ggplot2", depth = 2)
#' plot_dependencies(dd2, "circular")
#'
#' \dontrun{
#' plot_dependencies("deepdep", label_percentage = 0.5, depth = 2, local = TRUE)
#' }
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
plot_dependencies <- function(x, type = "circular", same_level = FALSE, reverse = FALSE,
                              label_percentage = 1, version = FALSE, ...) {
  UseMethod("plot_dependencies")
}

#' @rdname plot_deepdep
#' @export
plot_dependencies.default <- function(x, type = "circular", same_level = FALSE, reverse = FALSE,
                                      label_percentage = 1, version = FALSE, ...) {
  stop("This type of object does not have implemented method for 'plot_dependencies'")
}

#' @rdname plot_deepdep
#' @export
plot_dependencies.character <- function(x, type = "circular", same_level = FALSE, reverse = FALSE,
                                      label_percentage = 1, version = FALSE, ...) {
  dd <- deepdep(x, ...)
  plot_dependencies(dd, type, same_level, reverse, label_percentage, version, ...)
}

#' @rdname plot_deepdep
#' @export
plot_dependencies.deepdep <- function(x, type = "circular", same_level = FALSE, reverse = FALSE,
                                      label_percentage = 1, version = FALSE, ...) {
  # Due to NSE inside of the function, we have to declare "labeled" as NULL to prevent check fail
  labeled <- NULL

  if (nrow(x) == 0) {
    G <- make_graph(edges = c(), n = 1)
    G <- set_vertex_attr(G, "name", value = attr(x, "package_name"))
    type <- "tree"
  } else {
    if (version) {
      x <- add_version_to_name(x)
    }
    G <- graph_from_data_frame(x)
  }

  G <- add_layers_to_vertices(G)
  if (!same_level) {
    G <- delete_edges_within_layer(G)
  }
  if (!reverse) {
    G <- delete_reverse_edges(G)
  }


  # mark vertices to label
  pkg_downloads <- unlist(x[!duplicated(x[["name"]]), "grand_total"])
  # central node should always be labeled
  V(G)$labeled <- c(TRUE, pkg_downloads >= quantile(pkg_downloads, probs = 1 - label_percentage))
  labels <- levels(factor(E(G)$type))

  g <- switch(type,
    tree = ggraph(G, "tree"),
    circular = ggraph(graph = G, layout = "focus", focus = 1) +
      draw_circle(use = "focus", max.circle = max(V(G)$layer - 1), col = "#252525"))

  if (nrow(x) != 0) {
    g <- g + geom_edge_link(aes(end_cap = label_rect(node2.name),
                                start_cap = label_rect(node1.name),
                                edge_width = type,
                                edge_linetype = type),
                            arrow = arrow(length = unit(0.5, 'lines'),
                                          ends = "first",
                                          type = "closed",
                                          angle = 16.6),
                            color = "#1f271b") +
    scale_edge_linetype_manual(values = get_edgelinetype_default_scale()) +
    scale_edge_width_manual(values = get_edgewidth_default_scale()) +
    theme(legend.key.width = unit(3, "lines"))
  }

  g <- g + geom_node_point(aes(fill = factor(layer)),
                           size = 3, shape = 21, show.legend = FALSE) +
    geom_node_label(aes(label = ifelse(labeled, names(V(G)), ""), fill = factor(layer)),
                    show.legend = FALSE,
                    label.padding = unit(0.28, "lines")) +
    scale_fill_manual(values = get_nodefill_default_scale()) +
    coord_fixed() +
    theme_void()

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

#' @title Remove edges that points from vertice on lower to higher level
#' @noRd
#'
#' @param G An \code{igraph} object.
delete_reverse_edges <- function(G) {
  edges_to_delete <- E(G)[
    head_of(G, E(G))$layer < tail_of(G, E(G))$layer]
  delete_edges(G, edges_to_delete)
}

get_edgewidth_default_scale <- function() {
  c(Depends = unit(0.8, "lines"),
    Imports = unit(0.8, "lines"),
    Enhances = unit(0.5, "lines"),
    Suggests = unit(0.35, "lines"),
    LinkingTo = unit(0.55, "lines"))
}

get_edgelinetype_default_scale <- function() {
  c(Depends = "solid",
    Imports = "F1",
    Enhances = "longdash",
    Suggests = "dotted",
    LinkingTo = "dotdash")
}

get_nodefill_default_scale <- function() {
  c("#5fc8f4",
    "#a1ce40",
    "#fde74c",
    "#ff8330",
    "#e55934")
}

add_version_to_name <- function(x) {
   tmp <- x[!duplicated(x$name), c("name", "version")]
   nv <- ifelse(is.na(tmp$version), tmp$name, paste0(tmp$name, "\n(", tmp$version, ")"))
   names(nv) <- tmp$name
   x$name <- nv[x$name]
   x
}
