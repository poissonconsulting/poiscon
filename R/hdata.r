
#' @export
hdata <- function (data, offset = -8, unit = NULL, sname = NULL) {

  offset <- as.integer(offset)
  
  stopifnot (is.data.frame (data))
  stopifnot (is.integer (offset))
  stopifnot (is.null(unit) | is.character (unit))
  stopifnot (is.null(sname) | is.character (sname))

  stopifnot (nrow (data) != 0)
  stopifnot (length (offset) == 1)
  stopifnot (length (unit) %in% 0:1)
  stopifnot (length (sname) %in% 0:1)
  
  data <- subset (data, select = c("DateTime","value"))
  
  stopifnot(colnames(data) == c("DateTime","value"))
  
  stopifnot(lubridate::is.POSIXt(data$DateTime))
  stopifnot(is.numeric(data$value))
    
  object <- structure(data,
    offset = offset,
    unit = unit,
    sname = sname,
    class = c("data.frame","hdata")
  )
  return (object)
}

is.hdata <- function (x) {
  return ("hdata" %in% class (x))
}
