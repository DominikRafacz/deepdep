deepdep <- function(...) {
  reset_cached_files("ava")
  reset_cached_files("deps")
  reset_cached_files("desc")
  
  deepdep::deepdep(...)
}
