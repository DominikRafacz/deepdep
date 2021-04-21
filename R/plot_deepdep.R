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
#' @param show_version A \code{logical}. If \code{TRUE} required version of package will be
#' displayed below package name. Defaults to \code{FALSE}.
#' @param show_downloads A \code{logical}. If \code{TRUE} total number of downloads of packages
#' will be displayed below package names. Defaults to \code{FALSE}.
#' @param ... Other arguments passed to the \code{deepdep} function.
#'
#' @return A \code{ggplot2, gg, ggraph, deepdep_plot} class object.
#'
#' @examples
#'
#' \donttest{
#' library(deepdep)
#'
#' #:# use local packages
#' plot_dependencies("deepdep", depth = 2, local = TRUE)
#'
#' dd <- deepdep("ggplot2")
#' plot_dependencies(dd, "tree")
#'
#' dd2 <- deepdep("ggplot2", depth = 2)
#' plot_dependencies(dd2, "circular")
#'
#' #:# show grand_total download count
#' plot_dependencies("shiny", show_downloads = TRUE)
#' }
#'
#' @importFrom ggforce geom_circle
#' @importFrom graphlayouts draw_circle
#' @importFrom stats quantile reshape
#' @importFrom utils packageVersion
#' @import ggplot2
#' @import ggraph
#' @import igraph
#'
#' @rdname plot_deepdep
#' @export
plot_dependencies <- function(x, type = "circular", same_level = FALSE, reverse = FALSE,
                              label_percentage = 1, show_version = FALSE, show_downloads = FALSE, ...) {
  UseMethod("plot_dependencies")
}

#' @rdname plot_deepdep
#' @export
plot_dependencies.default <- function(x, type = "circular", same_level = FALSE, reverse = FALSE,
                                      label_percentage = 1, show_version = FALSE, show_downloads = FALSE, ...) {
  stop("This type of object does not have implemented method for 'plot_dependencies'")
}

#' @rdname plot_deepdep
#' @export
plot_dependencies.character <- function(x, type = "circular", same_level = FALSE, reverse = FALSE,
                                      label_percentage = 1, show_version = FALSE, show_downloads = FALSE, ...) {
  package_name <- NULL
  if (show_downloads == TRUE || label_percentage < 1)
    dd <- deepdep(x, downloads = TRUE, ...)
  else dd <- deepdep(x, ...)
  plot_dependencies(dd, type, same_level, reverse, label_percentage, show_version, show_downloads)
}

#' @rdname plot_deepdep
#' @export
plot_dependencies.deepdep <- function(x, type = "circular", same_level = FALSE, reverse = FALSE,
                                      label_percentage = 1, show_version = FALSE, show_downloads = FALSE, ...) {
  # Due to NSE inside of the function, we have to declare "labeled" as NULL to prevent check fail
  labeled <- NULL
  node1.name <- NULL
  node2.name <- NULL
  name <- NULL


  if ((label_percentage < 1 || show_downloads == TRUE) && !("grand_total" %in% colnames(x)))
    stop("When you use 'label_percentage' or 'show_downloads'",
         " you have to pass deepdep object with 'grand_total' column")

  if (nrow(x) == 0) {
    G <- make_graph(edges = c(), n = 1)
    G <- set_vertex_attr(G, "name", value = attr(x, "package_name"))
    type <- "tree"
  } else {
    if (show_version) {
      x <- add_version_to_name(x)
    }
    if (show_downloads) {
      x <- add_downloads_to_name(x)
    }
    G <- graph_from_data_frame(x)
  }

  G <- add_layers_to_vertices(G, x)
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
    tree = ggraph(G, "tree") +
      theme_void(),
    circular = ggraph(graph = G, layout = "focus", focus = 1) +
      draw_circle(use = "focus", max.circle = max(V(G)$layer), col = "#252525") +
      theme_void() +
      coord_fixed())

  if (nrow(x) != 0) {
    g <- g + geom_edge_link(aes(end_cap = label_rect(node2.name),
                                start_cap = label_rect(node1.name),
                                edge_width = type,
                                edge_linetype = type),
                                #edge_color = reverse),
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
    geom_node_label(data = function(g) g[g[, "labeled"], ],
                    aes(label = name, fill = factor(layer)),
                    show.legend = FALSE,
                    label.padding = unit(0.28, "lines")) +
    default_nodefill_scale(length(levels(factor(V(G)$layer)))) +
    labs(caption = paste0("Plot made with deepdep v",
                          packageVersion("deepdep"),
                          " on ", format(Sys.time(), usetz = FALSE))
)

  class(g) <- c(class(g), "deepdep_plot")
  g
}

#' @title Add layer property to graph vertices
#' @noRd
#'
#' @param G An \code{igraph} object.
add_layers_to_vertices <- function(G, x) {
  V(G)$layer <- c(0, x[match(V(G)$name[-1], x$name), "dest_level"])
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
  # rev_inds <-
  #   head_of(G, E(G))$layer < tail_of(G, E(G))$layer
  edges_to_delete <- E(G)[
    head_of(G, E(G))$layer < tail_of(G, E(G))$layer]
  delete_edges(G, edges_to_delete)
  # E(G)$reverse <- FALSE
  # E(G)$reverse[rev_inds] <- TRUE
  # G
}

get_edgewidth_default_scale <- function() {
  c(Depends = unit(1, "lines"),
    Imports = unit(0.8, "lines"),
    Enhances = unit(0.6, "lines"),
    Suggests = unit(0.4, "lines"),
    LinkingTo = unit(0.4, "lines"))
}

get_edgelinetype_default_scale <- function() {
  c(Depends = "solid",
    Imports = "F1",
    Enhances = "longdash",
    Suggests = "dotted",
    LinkingTo = "dotdash")
}

default_nodefill_scale <- function(num_colors) {
  if (num_colors <= 7)
    scale_fill_manual(values = c("#5fc8f4",
                                 "#a1ce40",
                                 "#fde74c",
                                 "#ff8330",
                                 "#e55934",
                                 "#7b5e7b",
                                 "#664e4c"))
  else scale_fill_discrete()
}

add_version_to_name <- function(x) {
   tmp <- x[!duplicated(x$name), c("name", "version")]
   nv <- ifelse(is.na(tmp$version), tmp$name, paste0(tmp$name, "\n(", tmp$version, ")"))
   names(nv) <- tmp$name
   x$name <- nv[x$name]
   x
}

add_downloads_to_name <- function(x) {
   tmp <- x[!duplicated(x$name), c("name", "grand_total")]
   nv <- ifelse(is.na(tmp$grand_total), tmp$name, paste0(tmp$name, "\n", tmp$grand_total))
   names(nv) <- tmp$name
   x$name <- nv[x$name]
   x
}
