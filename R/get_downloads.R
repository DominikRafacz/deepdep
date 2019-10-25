#' @title Scrap the download data of the package 
#' 
#' @description Download logs from the RStudio CRAN mirror through [META CRAN Logs](https://cranlogs.r-pkg.org/)
#' 
#' @param package A \code{character} vector. Names of packages that are on CRAN
#' 
#' @return An object of \code{package_download} class 
#' 
#' @author Hubert Baniecki, Szymon Maksymiuk
#' 
#' @examples
#' library(deepdep)
#' 
#' downloads <- get_downloads("ggplot2")
#' downloads
#' 
#'
#' @export
get_downloads <- function(package) {
  
  # check name
  
  # use the official `cranlogs` package for the querry
  downloads <- cranlogs::cran_downloads(package, from = "1996-01-01", to = Sys.Date())
  
  # remove zeros from last few days without data
  downloads_count <- rev(downloads$count)
  downloads_count <- downloads_count[which(downloads_count != 0)[1]:length(downloads_count)]
  
  # take count from last day that was not 0 ( cran_downloads(when = "last-day") )
  last_day <- downloads_count[1]
  last_week <- sum(downloads_count[1:7])
  last_month <- sum(downloads_count[1:30])
  last_quarter <- sum(downloads_count[1:91])
  last_half <- sum(downloads_count[1:182])
  grand_total <- sum(downloads_count)
  
  
  ret <- list("last_day" = last_day, "last_week" = last_week, "last_month" = last_month,
              "last_quarter" = last_quarter, "last_half" = last_half, "grand_total" = grand_total)
  
  class(ret) <- c("package_downloads", "list")
  ret
}

#' @rdname get_downloads
#' @export
print.package_downloads <- function(x, ...) {
  print(as.data.frame(x))
}
