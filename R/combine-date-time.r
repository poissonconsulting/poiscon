#

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
