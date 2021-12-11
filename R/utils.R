`%p%` <- function(lhs, rhs) {
  rhs <- substitute(rhs)
  len <- length(rhs)
  if (len == 1) rhs[[2]] <- lhs
  else {
    rhs[3:(len + 1)] <- rhs[2:len]
    names(rhs)[2:(len + 1)] <- c(NA, names(rhs)[2:len])
    rhs[[2]] <- lhs
  }
  eval(rhs)
}
