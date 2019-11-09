#' @title Run gravity iteration
#' 
#' @param layout Data frame object containing data about current layout.
#' @param G Graph which layout is being adjusted.
gravity_iteration <- function(layout, G) {
  for (i in 1:nrow(layout)) {
    push <- gravity_node(layout[i, ], layout, G)
    layout[i, "x_push"] <- push["x_push"]
    layout[i, "y_push"] <- push["y_push"]
  }
  moved_nodes <- layout[["layer"]] != 1
  # pushing nodes
  layout[moved_nodes, "x"] <- (layout[moved_nodes, "x"] + layout[moved_nodes, "x_push"])
  layout[moved_nodes, "y"] <- (layout[moved_nodes, "y"] + layout[moved_nodes, "y_push"])
  # moving nodes to correct orbits
  d <- sqrt(layout[["x"]]^2 + layout[["y"]]^2)
  layout[moved_nodes, "x"] <- layout[moved_nodes, "x"] *
    ((layout[moved_nodes, "layer"] - 1)/max(layout[["layer"]] - 1)) / d[moved_nodes]
  layout[moved_nodes, "y"] <- layout[moved_nodes, "y"] *
    ((layout[moved_nodes, "layer"] - 1)/max(layout[["layer"]] - 1)) / d[moved_nodes]
  layout
}

#' @title Compute gravity on a node
#' 
#' @param node A row from \code{layout} data frame with node to gravitate.
#' @param layout Data frame object containing data about current layout.
#' @param G Graph which layout is being adjusted.
#' 
#' @import igraph
gravity_node <- function(node, layout, G) {
  # Due to NSE inside of the function, we have to decleare "to" and "from" as NULL to prevent check fail
  to <- from <- NULL
  
  vertex <- node[["ggraph.orig_index"]]
  push <- c(x_push = 0, y_push = 0)
  # if it's central node, ignore it, it ain't movin'
  if (node[["layer"]] != 1) {
    # find relatives
    parents <- layout[tail_of(G, E(G)[to(vertex)]), ]
    children <- layout[head_of(G, E(G)[from(vertex)]), ]
    siblings <- layout[(layout[["layer"]] == node[["layer"]]) &
                         layout[["ggraph.index"]] != node[["ggraph.index"]], ]
    if (nrow(parents) > 0) {
      for (i in 1:nrow(parents)) {
        push <- push + gravity_parent(node, parents[i, ])
      }
    }
    if (nrow(children) > 0) {
      for (i in 1:nrow(children)) {
        push <- push + gravity_child(node, children[i, ])
      }
    }
    if (nrow(siblings) > 0) {
      for (i in 1:nrow(siblings)) {
        push <- push + gravity_sibling(node, siblings[i, ])
      }
    }
  }
  push
}

#' @title Push child towards the parent
#' 
#' @param node A row from \code{layout} data frame with node to push towards
#' the parent.
#' @param parent_node A row from \code{layout} data frame with parent node.
gravity_parent <- function(node, parent_node) {
  # computing distance between nodes
  d <- sqrt((parent_node[["x"]] - node[["x"]])^2 + (parent_node[["y"]] - node[["y"]])^2)
  x_push <- (parent_node[["x"]] - node[["x"]]) * d^2 / 5
  y_push <- (parent_node[["y"]] - node[["y"]]) * d^2 / 5
  c(x_push = x_push, y_push = y_push)
}

#' @title Push parent towards the child
#' 
#' @param node A row from \code{layout} data frame with node to push towards
#' the child.
#' @param child_node A row from \code{layout} data frame with child node.
gravity_child <- function(node, child_node) {
  d <- sqrt((child_node[["x"]] - node[["x"]])^2 + (child_node[["y"]] - node[["y"]])^2)
  x_push <- (child_node[["x"]] - node[["x"]]) * d^2 / 15
  y_push <- (child_node[["y"]] - node[["y"]]) * d^2 / 15
  c(x_push = x_push, y_push = y_push)
}

#' @title Push parent from the sibling
#' 
#' @param node A row from \code{layout} data frame with node to push from the
#' sibling.
#' @param sibling_node A row from \code{layout} data frame with sibling node.
gravity_sibling <- function(node, sibling_node) {
  d <- sqrt((sibling_node[["x"]] - node[["x"]])^2 + (sibling_node[["y"]] - node[["y"]])^2)
  x_push <- -(sibling_node[["x"]] - node[["x"]]) * d^(-2)
  y_push <- -(sibling_node[["y"]] - node[["y"]]) * d^(-2)
  c(x_push = x_push, y_push = y_push)
}
