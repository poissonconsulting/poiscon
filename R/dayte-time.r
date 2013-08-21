
#' @export
dayte_time<-function (dt, year = 2000, month = 1) {
  UseMethod("dayte_time", dt)
}

#' @export
dayte_time.integer <- function(dt, year = 2000, month = 1) {
  if(length(month) != 1)
    stop("month must be an integer of length 1")
  if (!month %in% 1:12)
    stop("month must be an integer between 1 and 12")
  
  x <- as.POSIXct(as.Date(paste(year,"01","01", sep = '-')),tz="UTC")
  x <- x + dt
  x <- with_tz(x,tz = "UTC")
    
  if (month == 1)
    return (x)
  
  yr <- lubridate::year(x)
  bol <- lubridate::month(x) >= month
  yr[bol] <- yr[bol] - 1
  
  return (as.POSIXct(as.Date(paste(yr, format(x, format='%m-%d'), sep = '-')),tz="UTC"))
}

#' @export
dayte_time.numeric <- function(dt, year = 2000, month = 1) {
  if(length(month) != 1)
    stop("month must be an integer of length 1")
  if (!month %in% 1:12)
    stop("month must be an integer between 1 and 12")
  
  x <- as.POSIXct(as.Date(paste(year,"01","01", sep = '-')),tz="UTC")
  x <- x + dt
  x <- with_tz(x,tz = "UTC")
  
  if (month == 1)
    return (x)
  
  yr <- lubridate::year(x)
  bol <- lubridate::month(x) >= month
  yr[bol] <- yr[bol] - 1
  
  return (as.POSIXct(as.Date(paste(yr, format(x, format='%m-%d'), sep = '-')),tz="UTC"))
}

#' @export
dayte_time.Date <- function(dt, year = 2000, month = 1) {
  if(length(month) != 1)
    stop("month must be an integer of length 1")
  if (!month %in% 1:12)
    stop("month must be an integer between 1 and 12")
  
  x <- as.POSIXct(as.Date(paste(year, format(dt, format='%m-%d'), sep = '-')),tz="UTC")
  
  if (month == 1)
    return (x)
  
  yr <- lubridate::year(x)
  bol <- lubridate::month(x) >= month
  yr[bol] <- yr[bol] - 1
  
  return (as.POSIXct(as.Date(paste(yr, format(dt, format='%m-%d'), sep = '-')),tz="UTC"))
}

#' @export
dayte_time.POSIXct <- function (dt, year = 2000, month = 1) {
  if(length(month) != 1)
    stop("month must be an integer of length 1")
  if (!month %in% 1:12)
    stop("month must be an integer between 1 and 12")
  
  x <- as.POSIXct(paste(year,format(dt, format = "%m-%d %H:%M:%S"), sep = '-'),format = "%Y-%m-%d %H:%M:%S",tz="UTC")
  
  if (month == 1)
    return (x)
  
  yr <- lubridate::year(x)
  bol <- lubridate::month(x) >= month
  yr[bol] <- yr[bol] - 1
  
  return (as.POSIXct(paste(yr,format(dt, format = "%m-%d %H:%M:%S"), sep = '-'),format = "%Y-%m-%d %H:%M:%S",tz="UTC"))
}

#' @export
dayte_time.POSIXlt <- function (dt, year = 2000, month = 1) {
  if(length(month) != 1)
    stop("month must be an integer of length 1")
  if (!month %in% 1:12)
    stop("month must be an integer between 1 and 12")
  
  x <- as.POSIXct(paste(year,format(dt, format = "%m-%d %H:%M:%S"), sep = '-'),format = "%Y-%m-%d %H:%M:%S",tz="UTC")
  
  if (month == 1)
    return (x)
  
  yr <- lubridate::year(x)
  bol <- lubridate::month(x) >= month
  yr[bol] <- yr[bol] - 1
  
  return (as.POSIXct(paste(yr,format(dt, format = "%m-%d %H:%M:%S"), sep = '-'),format = "%Y-%m-%d %H:%M:%S",tz="UTC"))  
}
