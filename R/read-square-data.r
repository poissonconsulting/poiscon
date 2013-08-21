
#' read_square_data
#'
#' Read square data into R
#' 
#' @param file the file with the square data is comma separated format
#' @return the data in the form of a object of class hdata
#' @export
read_square_data <- function (file="data.csv")
{
  if (!is.character(file)) 
    stop ("file must be class character")

  if (!length(file) == 1) 
    stop ("file must contain a single element")

  data <- read.csv(file)
  
  nrow <- nrow(data)
  years <- as.integer(substr(colnames(data),2,5))
  
  if (!nrow %in% c(365,366,8760,8784))
    stop("data should be hourly or daily by year")
  
  if (!all(years %in% 1900:format(Sys.Date(),format="%Y")))
    stop("column names should all be years")
  
  if (nrow == 365) {
    data$DateTime <- seq(ISOdate(2001,1,1),by="day", length.out = nrow)
  } else if (nrow == 366) {
    data$DateTime <- seq(ISOdate(2000,1,1),by="day", length.out = nrow)  
  } else if (nrow == 8760) {
    data$DateTime <- seq(ISOdate(2001,1,1,0),by="hour", length.out = nrow)  
  } else {
    data$DateTime <- seq(ISOdate(2000,1,1,0),by="hour", length.out = nrow)  
  }
  
  data <- reshape2::melt(data, id.vars = "DateTime", variable.name = "Year")
  
  data$Year <- as.integer(substr(data$Year,2,5))
  
  data$DateTime <- ISOdate(
    data$Year, 
    lubridate::month(data$DateTime), 
    lubridate::day(data$DateTime), 
    lubridate::hour(data$DateTime)
  )
  
  data <- na.omit(data)
  
  
  data <- subset(data, select=c("DateTime","value"))
  
  data <- data[!data[,"value",drop=T] %in% c(-9999),]
  
  row.names(data) <- 1:nrow(data)

  hdata <- hdata (data)
  
  return (hdata)
}


