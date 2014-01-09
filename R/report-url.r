#' @title Poisson report URL
#'
#' @description
#' Returns the URL of the poisson report corresponding to the
#' current project and date of posting.
#'
#' @param date Date scalar of when the report was posted.
#' @return The url of the poisson report.
#' @export
report_url <- function (date) {
  
  assert_that(is.Date(date))
  assert_that(is.scalar(date))
  
  url <- paste0("http://poissonconsulting.github.io/analyses/",
               format(as.Date("2014-01-09"),"%Y/%m/%d/"),
               project_folder(),
               ".html")
  return (url)
}
