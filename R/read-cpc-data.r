
read_cpc_file <- function (file) 
{
  dat <- read.csv (file)
  
  cols <- c("TIME","BRD_FLOWS_AVG","BRD_QSPILL_AVG","BRX_FLOW_AVG")
  ncols <- cols[!cols %in% colnames(dat)]
  
  if(length(ncols))
    stop(paste("columns",ncols,"are not in",file))
  
  dat <- subset(dat,select = cols)
  
  return (dat)
}

#' @title Read CPC data
#'
#' @description 
#' Reads CPC data into R
#' 
#' @param file the location of the zxrp file or folder with zxrp files
#' @param recursive a logical scalar indicating whether to read data from 
#' subdirectories
#' @param quiet a logical scalar indicating whether to provide messages 
#' @return the data in the form of a data.frame
#' @export
read_cpc_data <- function (file, recursive = TRUE, quiet = TRUE)
{
  if (!is.character(file)) 
    stop ("file must be class character")
  
  if (!length(file) == 1) 
    stop ("file must contain a single element")
  
  if(substr(file,nchar(file)-3,nchar(file)) == ".csv") {
    data <- read_cpc_file (file)
  } else {
    files <- list.files(path = file, recursive = recursive, full.names = TRUE)
    
    files <- files[substr(files,nchar(files)-3,nchar(files)) == ".csv"]
    
    if(!quiet) {
      print(files[1])
    }
    
    data <- read_cpc_file(files[1])
    
    for (file in files[-1]) {
      if(!quiet) {
        print(file)
      }
      dat <- try(read_cpc_file(file))
      if(!inherits(dat, "try-error")) {
        data <- rbind(data, dat)
      } else {
        warning(paste(file,"was not inputted"))
      }
    }
  }  
  data$Timing <- as.POSIXct(as.character(data$TIME), format = "%Y-%m-%d %H:%M", tz = "UTC")
  
  data$TIME <- NULL
  
  data <- reshape2::melt(data,id.vars = c("Timing"), variable.name = "Variable", value.name = "Level")
  
  data$Level <- data$Level * 0.028316847
  
  data$Status_BCH <- NA
  
  data <- data[order(data$Timing, data$Variable),]
  
  if(anyDuplicated(data))
    warning("duplicated data")
  
  return (data)
  
}
