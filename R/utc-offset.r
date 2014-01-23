#' @title UTC Offset
#'
#' @description 
#' Calculates the offsets in hours between the time zones
#' of a date time object and the Coordinated Universal Time time zone.
#' 
#' @param x a date-time object
#' @return The offsets as an numeric vector.
#' @seealso \code{\link{standardised_date_time}} and \code{\link{combine_date_time}}
#' @export
utc_offset <- function (x) {
  difftime <- force_tz(x, "UTC") - x
  period <- suppressMessages(as.period(difftime))
  hour <- hour(period)
  return (hour)
}
