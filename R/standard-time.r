
#' @export
offset <- function (time) {
  tz <- format(time, format = "%Z")
  off <- rep(NA, length(tz))
  off[tz == "PDT"] <- -7
  off[tz == "PST"] <- -8
  return (off)
}

#' @export
standard_time <- function (time, offset = NULL, tzh = -8) {
  if(is.null(offset))
    offset <- offset(time)
  time <- force_tz(time, tzone = "UTC")
  time <- time - hours(offset - tzh)
  return (time)
}
