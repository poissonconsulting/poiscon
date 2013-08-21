
#' @export
spawn_year<-function (dt, month = 6) {
  UseMethod("spawn_year", dt)
}

#' @export
spawn_year.Date <- function(dt, month = 6) {
  if(length(month) != 1)
    stop("month must be an integer of length 1")
  if (!month %in% 2:12)
    stop("month must be an integer between 2 and 12")
  
  yr <- lubridate::year(dt)
  bol <- lubridate::month(dt) >= month
  yr[bol] <- yr[bol] - 1
  
  return (yr)
}

#' @export
spawn_year.POSIXct <- function (dt, year = 2000, month = 1) {
  if(length(month) != 1)
    stop("month must be an integer of length 1")
  if (!month %in% 2:12)
    stop("month must be an integer between 2 and 12")
  
  yr <- lubridate::year(dt)
  bol <- lubridate::month(dt) >= month
  yr[bol] <- yr[bol] - 1

  return (yr)
}

#' @export
spawn_year.POSIXlt <- function (dt, year = 2000, month = 1) {
  if(length(month) != 1)
    stop("month must be an integer of length 1")
  if (!month %in% 2:12)
    stop("month must be an integer between 2 and 12")
  
  yr <- lubridate::year(dt)
  bol <- lubridate::month(dt) >= month
  yr[bol] <- yr[bol] - 1

  return (yr)
}
