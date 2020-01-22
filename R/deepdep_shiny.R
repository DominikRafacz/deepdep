#' @title Run Shiny app
#'
#' @description This function runs shiny app that helps to produce nice deepdep plot.
#'
#' @export
deepdep_shiny <- function() {
  appDir <- system.file("shiny_app", package = "deepdep")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `deepdep`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}
