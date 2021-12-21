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
#' @param show_stamp A \code{logical}. If \code{TRUE} (the default) the package version and
#' plot creation time will be added
#' @param declutter A \code{logical}. If \code{TRUE} then all layers beyond the first one
#' ignore non-strong dependencies (i.e. "Suggests" and "Enhances"). This visualizes the
#' so-called "hard costs of weak suggests".
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
#' @importFrom stats quantile reshape
#' @importFrom utils packageVersion
#'
#' @rdname plot_deepdep
#' @export
plot_dependencies <- function(x, type = "circular", same_level = FALSE, reverse = FALSE,
                              label_percentage = 1, show_version = FALSE, show_downloads = FALSE,
                              show_stamp = TRUE, declutter = FALSE, ...) {
  UseMethod("plot_dependencies")
}

#' @rdname plot_deepdep
#' @export
plot_dependencies.default <- function(x, type = "circular", same_level = FALSE, reverse = FALSE,
                                      label_percentage = 1, show_version = FALSE, show_downloads = FALSE,
                                      show_stamp = TRUE, declutter = FALSE, ...) {
  stop("This type of object does not have implemented method for 'plot_dependencies'")
}

#' @rdname plot_deepdep
#' @export
plot_dependencies.character <- function(x, type = "circular", same_level = FALSE, reverse = FALSE,
                                        label_percentage = 1, show_version = FALSE, show_downloads = FALSE,
                                        show_stamp = TRUE, declutter = FALSE, ...) {
  package_name <- NULL
  if (show_downloads == TRUE || label_percentage < 1)
    dd <- deepdep(x, downloads = TRUE, ...)
  else dd <- deepdep(x, ...)
  plot_dependencies(dd, type, same_level, reverse, label_percentage, show_version, show_downloads, show_stamp, declutter)
}

#' @rdname plot_deepdep
#' @export
plot_dependencies.deepdep <- function(x, type = "circular", same_level = FALSE, reverse = FALSE,
                                      label_percentage = 1, show_version = FALSE, show_downloads = FALSE,
                                      show_stamp = TRUE, declutter = FALSE, ...) {
  # Due to NSE inside of the function, we have to declare "labeled" as NULL to prevent check fail
  labeled <- NULL
  node1.name <- NULL
  node2.name <- NULL
  name <- NULL
  layer <- NULL
  label <- NULL
  
  # TODO: add boolean returns to check if packages are available
  if (!require_packages(c("ggplot2", "ggraph", "igraph", "graphlayouts"),
                        use_case = "plot package dependencies"))
    stop("Missing necessary packages.")
  
  if ((label_percentage < 1 || show_downloads == TRUE) && !("grand_total" %in% colnames(x)))
    stop("When you use 'label_percentage' or 'show_downloads'",
         " you have to pass deepdep object with 'grand_total' column")
  
  if (nrow(x) == 0) {
    G <- igraph::make_graph(edges = c(), n = 1)
    G <- igraph::set_vertex_attr(G, "name", value = attr(x, "package_name"))
    G <- igraph::set_vertex_attr(G, "label", value = attr(x, "package_name"))
    G <- igraph::set_vertex_attr(G, "labeled", value = TRUE)
    type <- "tree"
  } else {
    if (declutter) {
      x <- deepdep_declutter(x)
    }
    if (!same_level) {
      x <- x[(x$origin_level != x$dest_level), ]
    }
    if (!reverse) {
      x <- x[(x$origin_level <= x$dest_level), ]
    }
    vertices <- compile_vertex_data(
      x, show_version, show_downloads, label_percentage
    )
    G <- igraph::graph_from_data_frame(x, vertices = vertices)
  }
  
  G <- add_layers_to_vertices(G, x)
  
  plt <- switch(type,
                tree = ggraph::ggraph(G, "tree") +
                  ggplot2::theme_void(),
                circular = ggraph::ggraph(graph = G, layout = "focus", focus = 1) +
                  graphlayouts::draw_circle(use = "focus", max.circle = max(igraph::V(G)$layer), col = "#252525") +
                  ggplot2::theme_void() +
                  ggplot2::coord_fixed())
  
  if (nrow(x) != 0) {
    plt <- plt + ggraph::geom_edge_link(ggplot2::aes(end_cap = ggraph::label_rect(node2.name),
                                                     start_cap = ggraph::label_rect(node1.name),
                                                     edge_width = type,
                                                     edge_linetype = type),
                                        arrow = ggplot2::arrow(length = ggplot2::unit(0.5, 'lines'),
                                                               ends = "first",
                                                               type = "closed",
                                                               angle = 16.6),
                                        color = "#1f271b") +
      ggraph::scale_edge_linetype_manual(values = get_edgelinetype_default_scale()) +
      ggraph::scale_edge_width_manual(values = get_edgewidth_default_scale()) +
      ggplot2::theme(legend.key.width = ggplot2::unit(3, "lines"))
  }
  
  plt <- plt + ggraph::geom_node_point(ggplot2::aes(fill = factor(layer)),
                                       size = 3, shape = 21, show.legend = FALSE) +
    ggraph::geom_node_label(data = function(g) g[g[, "labeled"], ],
                            ggplot2::aes(label = label, fill = factor(layer)),
                            show.legend = FALSE,
                            label.padding = ggplot2::unit(0.28, "lines")) +
    default_nodefill_scale(length(levels(factor(igraph::V(G)$layer))))
  if (show_stamp)
    plt <- plt + ggplot2::labs(caption = paste0("Plot made with deepdep v",
                                                packageVersion("deepdep"),
                                                " on ", format(Sys.time(), usetz = FALSE)))
  
  class(plt) <- c(class(plt), "deepdep_plot")
  plt
}

#' @title Add layer property to graph vertices
#' @noRd
#'
#' @param G An \code{igraph} object.
add_layers_to_vertices <- function(G, x) {
  igraph::V(G)$layer <- igraph::distances(G, 1, mode = "out")
  G
}

compile_vertex_data <- function(x, version, downloads, label_percentage) {
  vertices <- data.frame(
    name = unique(c(attr(x, "package_name"), x[["name"]]))
  )
  vertices[["label"]] <- vertices[["name"]]
  vertices[["version"]] <- vapply(
    vertices[["name"]],
    function(pkg) x[x[["name"]] == pkg, ][["version"]][1],
    FUN.VALUE = character(1)
  )
  if (version) {
    vertices[["label"]] <- ifelse(
      is.na(vertices[["version"]]),
      vertices[["label"]],
      paste0(vertices[["label"]], "\n(", vertices[["version"]], ")")
    )
  }
  if ("grand_total" %in% colnames(x)) {
    vertices[["downloads"]] <- vapply(
      vertices[["name"]],
      function(pkg) x[x[["name"]] == pkg, ][["grand_total"]][1],
      FUN.VALUE = numeric(1)
    )
  }
  if (downloads) {
    vertices[["label"]] <- ifelse(
      is.na(vertices[["downloads"]]),
      vertices[["label"]],
      paste0(vertices[["label"]], "\n", vertices[["downloads"]])
    )
  }
  vertices[["labeled"]] <- if (label_percentage < 1) {
    # Central node should always be labeled
    c(TRUE, is_top_perc(vertices[["downloads"]][-1], label_percentage))
  } else {
    TRUE
  }
  vertices
}

get_edgewidth_default_scale <- function() {
  c(Depends = ggplot2::unit(1, "lines"),
    Imports = ggplot2::unit(0.8, "lines"),
    Enhances = ggplot2::unit(0.6, "lines"),
    Suggests = ggplot2::unit(0.4, "lines"),
    LinkingTo = ggplot2::unit(0.4, "lines"))
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
    ggplot2::scale_fill_manual(values = c("#5fc8f4",
                                          "#a1ce40",
                                          "#fde74c",
                                          "#ff8330",
                                          "#e55934",
                                          "#7b5e7b",
                                          "#664e4c"))
  else ggplot2::scale_fill_discrete()
}
