
# converts all POSIX datetimes to UTC-0800
UTC8<-function (d)
{
  gmt<-tz(d)=='GMT'
  d<-with_tz(d,'UTC')
  if(any(!gmt))
    d[!gmt]<-d[!gmt]-hours(8)
  d<-force_tz(d,'UTC')
  return (d)
}

expand_unique <- function (x, select = NULL, droplevels = FALSE) {
  stopifnot(is.data.frame(x))
  
  if(!is.null(select))
    x <- subset(x,select = select)
  
  data <- list()
  for (colname in colnames(x)) {
    vec <- x[[colname]]
    if (!is.factor(vec)) {
      data[colname] <- sort(unique(vec))
    } else {
      if (droplevels) 
        vec <- droplevels(vec)
      data[colname] <- levels(vec)
    }
  }
  data <- expand.grid(data)
  return (data)
}



do_plot_analysis<-function (name, species, plot_analysis) {
  
  if(!is.function (plot_analysis))
    stop("plot_analysis should be a function")
  
  
  for (spp in species)
  {
    cat(paste0("\nSpecies: ",spp,"\n"))
    
    graphics.off()
    
    set_folders(name, spp)
    
    analysis <- load_analysis()
    
    print(summary(analysis))
    plot_analysis (analysis)
    
    save_tables(analysis)
    save_plots(analysis)
  }
}

get_spp <- function () {
  folder <- getOption ("folders.analyses_folder")
  spp <- strsplit(folder, "/")[[1]][2]
  return (spp)
}

do_plot_analyses<-function (name, species, plot_analysis) {
  
  if(!is.function (plot_analysis))
    stop("plot_analyses should be a function")
  type <- getOption ('Poisson.analysis_type','full')
  
  if (type %in% c('first','debug')) {
    species <- sort(species)[1]
  } else if (type == 'remainder') {
    species <- species[species != sort(species)[1]]
  }
  
  for (spp in species)
  {
    print(spp)
    graphics.off()
    
    set_folders(name, spp)
    
    analysis<-load_analysis()
    
    print(summary(analysis))
    save_tables(analysis)
    save_plots(analysis)
    
    plot_analysis (analysis)
  }
}

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
  
  bol <- FALSE #WriteXLS(data, file)
  
  if(!bol)
    warning("write-hdata failed")
  
  invisible (bol)
}

write_bch_data <- function (data, file = NULL)
{
  
  if(is.null(file))
    file <- format(min(data$Timing), format = "%Y-%m-%d")
  
  file <- paste(file,"csv",sep=".")
  
  invisible(write.csv(data,file,row.names = FALSE))
}

is_predictive <- function (x) {
  
  if(!is.data.frame(x))
    stop("x should be class data.frame")
  
  if(!"estimate" %in% colnames(x))
    stop("estimate should be a column in x")
  
  return (diff(range(x$estimate)) > 0)
}

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

#' read_square_data
#'
#' Read square data into R
#' 
#' @param file the file with the square data is comma separated format
#' @return the data in the form of a object of class hdata
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
  dat$Timing <- as.POSIXct(as.character(dat$Timing),tz = "UTC")
  
  dat$Timing <- dat$Timing - (8 + offset) * 3600
  
  dat$Variable <- variable
  dat$Logger <- logger
  
  dat <- dat[order(dat$Timing, dat$Variable),]
  dat <- na.omit(dat)
  
  return (dat)
}


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

spawn_year<-function (dt, month = 6, ...) {
  UseMethod("spawn_year", dt)
}

spawn_year.Date <- function(dt, month = 6, ...) {
  if(length(month) != 1)
    stop("month must be an integer of length 1")
  if (!month %in% 2:12)
    stop("month must be an integer between 2 and 12")
  
  yr <- lubridate::year(dt)
  bol <- lubridate::month(dt) >= month
  yr[bol] <- yr[bol] - 1
  
  return (yr)
}

spawn_year.POSIXct <- function (dt, month = 1, ...) {
  if(length(month) != 1)
    stop("month must be an integer of length 1")
  if (!month %in% 2:12)
    stop("month must be an integer between 2 and 12")
  
  yr <- lubridate::year(dt)
  bol <- lubridate::month(dt) >= month
  yr[bol] <- yr[bol] - 1
  
  return (yr)
}

spawn_year.POSIXlt <- function (dt, month = 1, ...) {
  if(length(month) != 1)
    stop("month must be an integer of length 1")
  if (!month %in% 2:12)
    stop("month must be an integer between 2 and 12")
  
  yr <- lubridate::year(dt)
  bol <- lubridate::month(dt) >= month
  yr[bol] <- yr[bol] - 1
  
  return (yr)
}

standard_time <- function (time, offset = NULL, tzh = -8) {
  if(is.null(offset))
    offset <- offset(time)
  time <- force_tz(time, tzone = "UTC")
  time <- time - hours(offset - tzh)
  return (time)
}