#' @title Scrap the download data of the package
#'
#' @description This function uses \href{https://github.com/r-hub/cranlogs}{\bold{API}} of
#' \href{http://cran-logs.rstudio.com/}{\bold{CRAN Logs}} to scrap the download logs of the package.
#'
#' @param package A \code{character}. Name of the package that is on CRAN.
#'
#' @return An object of \code{package_downloads} class.
#'
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

  if (!is_available(package)) return(NULL)

  # use the `cranlogs` package for the querry (from offical cran downloads data)
  # count from 2012-10-01 - date of first entry "http://cran-logs.rstudio.com/"
  downloads <- cranlogs::cran_downloads(package, from = "2012-10-01", to = Sys.Date())

  # remove zeros from last few days without data
  downloads_count <- rev(downloads$count)

  # first two entries are 0
  # downloads_count <- downloads_count[which(downloads_count != 0)[1]:length(downloads_count)]

  downloads_count <- downloads_count[3:length(downloads_count)]

  # take count from last day that was not 0 ( cran_downloads(when = "last-day") )
  last_day <- downloads_count[1]
  last_week <- sum(downloads_count[1:7])
  last_month <- sum(downloads_count[1:30])
  last_quarter <- sum(downloads_count[1:91])
  last_half <- sum(downloads_count[1:182])
  grand_total <- sum(downloads_count)


  ret <- list("last_day" = last_day, "last_week" = last_week, "last_month" = last_month,
              "last_quarter" = last_quarter, "last_half" = last_half, "grand_total" = grand_total)

  attr(ret, "package_name") <- package
  class(ret) <- c("package_downloads", "list")
  ret
}

#' @title Print function for an object of \code{package_downloads} class
#'
#' @param x An object of \code{package_downloads} class.
#' @param ... other
#'
#'
#' @examples
#' library(deepdep)
#'
#' desc <- get_downloads("stringr")
#' desc
#'
#' @rdname print.package_downloads
#' @export
print.package_downloads <- function(x, ...) {
  print(as.data.frame(x))
}
