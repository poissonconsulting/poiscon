
#' @title Offset
#'
#' @description 
#' Offset.
#' 
#' @param time a datetime object
#' @export
offset <- function (time) {
  tz <- format(time, format = "%Z")
  off <- rep(NA, length(tz))
  off[tz == "PDT"] <- -7
  off[tz == "PST"] <- -8
  return (off)
}
