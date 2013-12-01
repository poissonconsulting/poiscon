
#' @title Read hobo data
#'
#' @description 
#' Reads hobo data into R
#' 
#' @param file the location of the hobo file or folder with hobo files
#' @param recursive a logical scalar indicating whether to read data from 
#' subdirectories
#' @param quiet a logical scalar indicating whether to provide messages 
#' @return the data in the form of a data.frame
#' @export
read_hobo_data <- function (file, recursive = TRUE, quiet = TRUE)
{
  if (!is.character(file)) 
    stop ("file must be class character")
  
  if (!length(file) == 1) 
    stop ("file must contain a single element")
  
  if(substr(file,nchar(file)-3,nchar(file)) == ".csv") {
    data <- read_hobo_file (file)
  } else {
    files <- list.files(path = file, recursive = recursive, full.names = TRUE)
    
    files <- files[substr(files,nchar(files)-3,nchar(files)) == ".csv"]
    
    if(!quiet) {
      print(files[1])
    }
    
    data <- read_hobo_file(files[1])
    
    for (file in files[-1]) {
      if(!quiet) {
        print(file)
      }
      dat <- try(read_hobo_file(file))
      if(!inherits(dat, "try-error")) {
        data <- rbind(data, dat)
      } else {
        warning(paste(file,"was not inputted"))
      }
    }
  }  
  data$Timing <- as.POSIXct(as.character(data$Timing), format = "%Y-%m-%d %H:%M", tz = "UTC")
  
  data$Logger <- NULL
  
  data$Status_BCH <- NA
  
  data <- data[order(data$Timing, data$Variable),]
  
  if(anyDuplicated(data))
    warning("duplicated data")
  
  return (data)
}
