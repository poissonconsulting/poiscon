# either hdata or just two columns...
write_data <- function (x, file = "data.xls", value.name = "Discharge", location = "") {
  stopifnot(is.data.frame (x))
  stopifnot(is.character (file))
  stopifnot(is.character (value.name))
  stopifnot(is.character (location))
  
  if (!is.hdata (x))
    x <- hdata (x)
  
  stopifnot(length (file) == 1)
  stopifnot(length (value.name) == 1)
  stopifnot(length (location) == 1)
  
  stopifnot(!value.name %in% c("DateTime", "Location"))
  stopifnot(substr(file, nchar(file) - 3, nchar(file)) == ".xls")
    
  data[,value.name] <- data$value
  
  data$value <- NULL
  
  data$Location <- location

  data <- subset(data, select= c("Location", "DateTime", value.name))
  
  print(class(data))
  
  bol <- WriteXLS::WriteXLS(data, file)
  
  if(!bol)
    warning("write-hdata failed")
  
  invisible (bol)
}
