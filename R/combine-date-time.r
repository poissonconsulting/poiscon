
#' @title Combine date and time
#'
#' @description
#' Combines date and time into a datetime object.
#' 
#' @param date coerced to as.Date
#' @param time coerced to as.POSIXct
#' @return A datetime object.
#' @export
combine_date_time<-function (date, time) {
  date <- as.Date(date)
  time <- as.POSIXct(time)
  
  datetime<- ISOdate(
    year = lubridate::year(date), 
    month = lubridate::month(date), 
    day = lubridate::day(date), 
    hour = lubridate::hour(time), 
    min = lubridate::minute(time), 
    sec = lubridate::second(time),
    tz = "GMT"
  )
  return (datetime)
}
