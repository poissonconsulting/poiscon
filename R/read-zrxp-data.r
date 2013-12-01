
#' @export
read_zrxp <- function (file, recursive = FALSE, quiet = TRUE)
{
  warning("read_zrxp function is deprecated - use read_zrxp_data function instead")
  return (read_zrxp_data(file = file, recursive = recursive, quiet = quiet))
}

#' @title Read zrxp data into R
#'
#' @description 
#' Reads zrxp data into R
#' 
#' @param file the location of the zxrp file or folder with zxrp files
#' @param recursive a logical scalar indicating whether to read data from 
#' subdirectories
#' @param quiet a logical scalar indicating whether to provide messages 
#' @return the data in the form of a data.frame
#' @aliases read_zrxp
#' @export
read_zrxp_data <- function (file, recursive = FALSE, quiet = TRUE)
{
  if (!is.character(file)) 
    stop ("file must be class character")
  
  if (!length(file) == 1) 
    stop ("file must contain a single element")
  
  if(substr(file,nchar(file)-4,nchar(file)) == ".zrxp")
    return (read_zrxp_file (file))
  
  files <- list.files(path = file, recursive = recursive, full.names = TRUE)
  
  files <- files[substr(files,nchar(files)-4,nchar(files)) == ".zrxp"]
  
  if(!quiet) {
    print(files[1])
  }
  
  data <- read_zrxp_file(files[1])
  
  for (file in files[-1]) {
    if(!quiet) {
      print(file)
    }
    dat <- try(read_zrxp_file(file))
    if(!inherits(dat, "try-error")) {
      data <- rbind(data, dat)
    } else {
      warning(paste(file,"was not inputted"))
    }
  }
  data <- data[order(data$Timing, data$Variable),]
  
  if(anyDuplicated(data))
    warning("duplicated data")
  
  return (data)
}

read_zrxp_file <- function (file)
{
  if (!is.character(file)) 
    stop ("file must be class character")
  
  if (!length(file) == 1) 
    stop ("file must contain a single element")
  
  if(substr(file,nchar(file)-4,nchar(file)) != ".zrxp")
    stop ("file must be a .zrxp file")
  
  con <- file(file, open = "r")
  lines <- readLines(con)
  close(con)
    
  meta <- grep("#REXCHANGE",lines,useBytes = TRUE) 
  meta2 <- grep("#ZRXPVERSION",lines,useBytes = TRUE) 
  
  if(length(meta2)) {
    meta <- meta2
  }
  
  dat <- grep("#LAYOUT\\(.+\\)\\|\\*\\|",lines,useBytes = TRUE) + 1 
  
  if(length(meta) == 0)
    stop("unrecognised format")
  
  if(length(dat) != length(meta))
    stop("unrecognised format")
  
  meta <- c(meta,length(lines)+1)
  
  ls <- list()
  for (i in 1:length(dat)) {
    ls[[i]] <- list(meta = lines[meta[i]:(dat[i]-1)],
                      data = lines[dat[i]:(meta[i+1]-1)])
  }
  
  process_meta<- function (ele) {
    
    x <- ele$meta
  
    Variable <- grep("#REXCHANGE.+",x,useBytes = TRUE, value = TRUE)
    Variable <- sub("#REXCHANGE","",Variable,useBytes = TRUE)
    Variable <- sub("\\|\\*\\|.+","",Variable,useBytes = TRUE)
    
    TZ <- grep(".+TZ.+",x,useBytes = TRUE, value = TRUE)
    
    if(length(TZ)) {
      TZ <- grep(".+TZ.+",x,useBytes = TRUE, value = TRUE)
      TZ <- sub(".+TZ","",TZ,useBytes = TRUE)
      TZ <- sub("\\|\\*\\|","",TZ,useBytes = TRUE)
      if (TZ != "UTC-8")
        stop("timezone is not UTC-8")
    } 
    
    ele$meta <- list(Variable = Variable)
    
    return (ele)
  }
  
  process_data <- function (ele) {

    x <- ele$data
    
    x <- stringr::str_split(x," ")
  
    ncol <- length(x[[1]])
    nrow <- length(x)
  
    x <- unlist(x)
  
    x <- matrix(x,nrow = nrow, ncol = ncol, byrow = T)
  
    x <- as.data.frame(x)

    if(ncol(x) == 2)
      x$Status_BCH <- NA
  
    colnames(x) <- c("Timing","Level","Status_BCH")
  
    x$Timing <- lubridate::ymd_hms(as.character(x$Timing),quiet = TRUE)
    x$Level <- as.double(as.character(x$Level))
    x$Status_BCH <- as.integer(as.character(x$Status_BCH))
    
    ele$data <- x
    
    return (ele)
  }
  
  merge_meta_data <- function (ele) {
    ele$data$Variable <- ele$meta$Variable
    
    return (ele$data)
  }
  
  ls <- lapply(ls,process_meta)  
  ls <- lapply(ls,process_data)
  ls <- lapply(ls,merge_meta_data)
  
  data <- plyr::rbind.fill(ls)
  
  data <- subset(data,select = c("Timing","Variable","Level","Status_BCH"))
  
  return (data)
}
