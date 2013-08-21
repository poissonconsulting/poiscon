#

#' @export
doy<-function (date) {
  date <- as.Date(date)
  as.integer(format(date,format='%j'))
}
