#' @title Run Shiny example for deepdep package
#'
#' @description This function runs Shiny app with example usage
#' of deepdep package.
#'
#' @export
run_shiny_example <- function() {
  appDir <- system.file("shiny_examples", "usage_app", package = "deepdep")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `deepdep`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
