
#' @title Diel period
#'
#' @description
#' Calculates whether a date/time is Day or Night based on the
#' latitude and longitude. The default is for the Poisson Consulting Ltd
#' offices.
#' 
#' @param x date/time
#' @param Lat a numeric element indicating the latitude.
#' @param Long a numeric element indicating the longitude.
#' @return A factor with the levels Day and Night.
#' @export
diel_period <- function(x, Lat = 49.49, Long=-117.3) {
    
  hours <- lubridate::hour(x) + lubridate::minute(x)/60 + lubridate::second(x)/ (60 * 60)
    
  suntimes <- sun_calc(d = yday(x), Lat = Lat, Long = Long)
  
  period <- factor(rep("Night",length(hours)),levels = c("Day","Night"))
  period[hours >= suntimes$sunrise & hours <= suntimes$sunset] <- "Day"
  
  return (period)
}
