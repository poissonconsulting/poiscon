#' @title UTC Offset
#'
#' @description 
#' Calculates the offsets in hours between the time zones
#' of a date time object and the Coordinated Universal Time time zone.
#' 
#' @param x a date-time object
#' @return The offsets as an numeric vector.
#' @export
utc_offset <- function (x) {
  difftime <- lubridate::force_tz(x, "UTC") - x
  period <- suppressMessages(lubridate::as.period(difftime))
  hour <- lubridate::hour(period)
  return (hour)
}

#' @title Offset
#'
#' @description 
#' Offset.
#' 
#' @param time a datetime object
#' @export
offset <- function (time) {
  warning("deprecated using utc_offset instead")
  tz <- format(time, format = "%Z")
  off <- rep(NA, length(tz))
  off[tz == "PDT"] <- -7
  off[tz == "PST"] <- -8
  return (off)
}
