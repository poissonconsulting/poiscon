

split_dayte <- function (dayte, year, month) {
  stopifnot(month %in% 1:12)
  if (month != 12) {
    bol <- month(dayte) > month
    if (month == 1) {
      dayte[!bol] <- as.Date(paste(year+1,month(dayte),day(dayte),sep="-"))[!bol] 
    } else {
      dayte[bol] <- as.Date(paste(year-1,month(dayte),day(dayte),sep="-"))[bol] 
    }
  }
  return (dayte)
}

#' @export
dayte<-function (dt, year = 2000, month = 0) {
  UseMethod("dayte", dt)
}

#' @export
dayte.numeric <- function(dt, year = 2000, month = 12) {
  dayte <- as.Date(paste0(year-1,"-12-31")) + dt
  dayte <- split_dayte(dayte,year,month)
  return (dayte)
}

#' @export
dayte.integer<- function(dt, year = 2000, month = 12) {
  dayte <- as.Date(paste0(year-1,"-12-31")) + dt
  dayte <- split_dayte(dayte,year,month)
  return (dayte)
}

#' @export
dayte.Date <- function(dt, year = 2000, month = 12) {
  dayte <- as.Date(paste(year, format(dt, format='%m-%d'), sep = '-'))
  dayte <- split_dayte(dayte,year,month)
  return (dayte)
}

#' @export
dayte.POSIXct <- function (dt, year = 2000, month = 12) {
  dayte <- as.Date(paste(year, format(dt, format='%m-%d'), sep = '-'))
  dayte <- split_dayte(dayte,year,month)
  return (dayte)
}

#' @export
dayte.POSIXlt <- function (dt, year = 2000, month = 12) {
  dayte <- as.Date(paste(year, format(dt, format='%m-%d'), sep = '-'))
  dayte <- split_dayte(dayte,year,month)
  return (dayte)
}
