get_cached_obj <- function(type, repo) {
  cache_dir <- tempdir()
  cache_file <- paste0(cache_dir, "/deepdep_", type, "_", repo, "_cache.RDS")
  if (!file.exists(cache_file) || is_too_old(cache_file)) {
    obj <- list()
    attr(obj, "type") <- type
    attr(obj, "repo") <- repo
    attr(obj, "new") <- TRUE
    saveRDS(obj, cache_file)
    obj
  } else {
    readRDS(cache_file)
  }
}

save_cache <- function(obj) {
  saveRDS(obj,
          paste0(tempdir(), "/deepdep_", attr(obj, "type"), "_", attr(obj, "repo"), "_cache.RDS"))
}

is_too_old <- function(file) {
   difftime(Sys.time(), file.info(file)$mtime, units = "secs") > 1800
}

reset_cached_files <- function(type) {
  cache_dir <- tempdir()
  cache_files <- paste0(cache_dir, "/deepdep_", type, "_", 
                       c("CRAN", "bioc", "local"), "_cache.RDS")
  for (cache_file in cache_file) {
    if (file.exists(cache_file)) file.remove(cache_file)
  }
}