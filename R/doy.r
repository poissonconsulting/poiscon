
#' @title Doy
#'
#' @description
#' Calculates day of the year.
#' 
#' @param date object to convert - coerced to class Date.
#' @return An integer vector indicating the day of the year from 1 - 366.
#' @seealso \code{\link{dayte}}
#' @export
doy<-function (date) {
  date <- as.Date(date)
  as.integer(format(date,format='%j'))
}
