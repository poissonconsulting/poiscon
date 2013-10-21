
#' @export
input_hobo_temperature_csv<-function (file, utc_offset = -8)
{
  data <- read.csv(file, header=F)

  data<-data[,1:3]
  
  if(data[1,1]!='#') {
    data<-data[-1,]	
  }
  logger<-as.numeric(strsplit(strsplit(as.character(data[1,3]),":")[[1]][2],")")[[1]][1])

  offset<-as.numeric(strsplit(strsplit(as.character(data[1,2]),"GMT")[[1]][2],":")[[1]][1])	
  
  data<-data[-1,2:3]
  colnames(data)<-c("DateTime","Temperature")
    
  data$Logger <- logger

  data$Temperature<-as.numeric(as.character(data$Temperature))
  
  data$DateTime <- as.character(data$DateTime)
      
  data$DateTime<- as.POSIXct(data$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")

  data$DateTime <- data$DateTime + (utc_offset - offset) * 60 * 60

  data <- data[order(data$DateTime),c("Logger","DateTime","Temperature")]
  
  data$File <- file

#  data <- na.omit(data)
     
  return (data)
}

#' @export
input_hobo_temperature_csvs <- function (path = ".", utc_offset = -8, recursive = FALSE, quiet = TRUE) {
  
  utc_offset <- as.integer(utc_offset)
  if (!utc_offset %in% -12:12)
    stop("utc_offset should be an integer between -12 and 12")

  files <- list.files(path = path, recursive = recursive, full.names = TRUE)
  
  files <- files[substr(files,nchar(files)-3,nchar(files)) == ".csv"]
  
  if(!quiet) {
    print(files[1])
  }
  data <- input_hobo_temperature_csv(files[1], utc_offset = utc_offset)
    
  for (file in files[-1]) {
    if(!quiet) {
      print(file)
    }
    dat <- try(input_hobo_temperature_csv(file, utc_offset = utc_offset))
    if(!inherits(dat, "try-error")) {
      data <- rbind(data, dat)
    } else {
      warning(paste(file,"was not inputted"))
    }
  }
  data <- data[order(data$DateTime, data$Logger),]
  return (data)
}
