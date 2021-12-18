is_top_perc <- function(x, perc) {
  is_top_n(x, ceiling(perc * length(x)))
}

is_top_n <- function(x, n) {
  seq_along(x) %in% order(x, decreasing = TRUE)[seq_len(n)]
}
