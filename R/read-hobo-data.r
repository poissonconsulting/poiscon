
read_hobo_file <- function (file) 
{
  dat <- read.csv (file, header = FALSE)
  
  variable <- strsplit(file,split="/")[[1]]
  variable <- variable[length(variable)-1]
  
  if(!variable %in% c("Lar23_GerrardViewingPlatform","Lar27_OtterRock","Lar50_Hwy50Sign"))
      stop(paste("variable",variable,"not recognised"))

  dat<-dat[,1:3]
  
  if(substr(dat[1,1],2,12) == "Plot Title:") {
    logger <- substr(dat[1,1],14,nchar(as.character(dat[1,1])))
    logger <- strsplit(logger,split="-")[[1]][2]
    dat<-dat[-1,]	
  } else {
    if(dat[1,1]!='#') {
      dat<-dat[-1,]	
    }
    logger<-as.numeric(strsplit(strsplit(as.character(dat[1,3]),":")[[1]][2],")")[[1]][1])
  }
  offset<-as.numeric(strsplit(strsplit(as.character(dat[1,2]),"GMT")[[1]][2],":")[[1]][1])
  
  dat<-dat[-1,2:3]
  colnames(dat)<-c("Timing","Level")

  dat$Level <- as.numeric(as.character(dat$Level))
  dat$Timing <- as.POSIXct(as.character(dat$Timing))
  dat$Timing <- dat$Timing - (8 + offset) * 3600
  
  dat$Variable <- variable
  dat$Logger <- logger

  dat <- dat[order(dat$Timing, dat$Variable),]
  dat <- na.omit(dat)
  
  return (dat)
}

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
