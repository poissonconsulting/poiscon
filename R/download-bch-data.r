
#' @title Download BCH data
#'
#' @description
#' Download data from environmental database
#' 
#' @param code a character vector of the station codes.
#' @param period a character element of the period by default "hourly" but can also be
#' "raw" and "daily".
#' @param startDate a Date element of the beginning of the time series.
#' @param endDate a Date element of the end of the time series. 
#' By default the current date.
#' @param status a character scalar indicating which type of values should be included.
#' @param surrogate a character scalar indicating which type of values should be repalced by a surrogate if available.
#' @param add_spaces a logical scalar indicating whether missing values should be 
#' filled in with NAs. 
#' @param access_file an character scalar indicating the path to the database.
#' @return A data.frame object.
#' @seealso \code{\link{dayte}}.
#' @export
download_bch_data <- function (code = "DDM", period = "hourly", 
      startDate = as.Date("2013-01-01"), endDate = Sys.Date(), status = "questionable", surrogate = "questionable", add_spaces = TRUE,
      access_file = "../../../bchydro_data_12/data/based/download/ColDuncEnv.accdb")
{ 
  if(.Platform$OS.type != "windows")
    stop("only available for windows")
  
  if (!inherits(code,"character"))
    stop("code must be class character")

  if (!inherits(period,"character"))
    stop("period must be class character")

  if (!inherits(startDate,"Date"))
    stop("startDate must be class Date")

  if (!inherits(endDate,"Date"))
    stop("endDate must be class Date")
  
  if (!inherits(status,"character"))
    stop("status must be class character")

  if (!inherits(surrogate,"character"))
    stop("surrogate must be class character")
  
  if (!inherits(add_spaces,"logical"))
    stop("add_spaces must be class logical")
  
  if(length(period) != 1)
    stop("period must be length one")

  if(length(startDate) != 1)
    stop("startDate must be length one")

  if(length(endDate) != 1)
    stop("endDate must be length one")
  
  if(length(status) != 1)
    stop("status must be length one")

  if(length(surrogate) != 1)
    stop("surrogate must be length one")
  
  if(length(add_spaces) != 1)
    stop("add_spaces must be length one")
  
  if(!period %in% c("raw","hourly","daily","annual"))
    stop("period must be raw, hourly, daily or annual")

  if(!status %in% c("reasonable","questionable","erroneous"))
    stop("status must be reasonable, questionable or erroneous")

  if(!surrogate %in% c("none","missing","erroneous","questionable","reasonable","duplicate"))
    stop("status must be none, missing, erroneous, questionable, reasonable or duplicate")
  
  if(endDate > Sys.Date()) {
    message("endDate set to current date")
    endDate <- Sys.Date()
  }
  
  if(startDate >= endDate)
    stop("startDate must be less than endDate")

  years <- lubridate::year(startDate):lubridate::year(endDate)
  
  status_arg <- status
  
  where <- "WHERE ("
  for (cod in code) {
    if(cod != code[1])
      where <- paste0(where," OR ")
    where <- paste0(where,"CODE = '",cod,"'", collapse = " ")
  }
  
  where <- paste0(where,") AND (")
  
  for (year in years) {
    if(year != years[1])
      where <- paste0(where," OR ") 
    where <- paste0(where,"Yr = ",year)
  }  
  where <- paste0(where,")")
  
  table_name <- switch(period,
                       raw = "Raw",
                       hourly = "Hourly",
                       daily = "Daily",
                       annual = "Yearly")
  
  sql <- paste("SELECT * FROM",table_name,where)
  
  db <- RODBC::odbcConnectAccess2007(access_file)
  
  on.exit(RODBC::odbcCloseAll())
  
  if(!nrow(RODBC::sqlTables(db, tableName = "Location")))
    stop("Location table is not defined")
  
  location <- RODBC::sqlFetch(db, sqtable = "Location", colnames = FALSE, rownames = TRUE)
    
  unrec <- unique(code[!code %in% location$Code])
  
  if(length(unrec))
    stop(c("the following codes are unrecognized:",unrec))
  
  location <- location[location$Code %in% code,]
  
  if(!nrow(RODBC::sqlTables(db, tableName = "Status")))
    stop("Status table is not defined")
  
  status <- RODBC::sqlFetch(db, sqtable = "Status", colnames = FALSE, rownames = TRUE)
  
  status$Status_Description <- status$Description
  
  data <- RODBC::sqlQuery(db,sql)
  
  if(is.null(data$Mon))
    data$Mon <- 1
  if(is.null(data$Dy))
    data$Dy <- 1
  if(is.null(data$Hr))
    data$Hr <- 0  
  if(is.null(data$Max)) {
    minutes <- data$Min  
    data$Min <- data$Level
    data$Mean <- data$Level
    data$Max <- data$Level    
  } else 
    minutes <- 0    
  
  data$Timing <- ISOdatetime(data$Yr, data$Mon, data$Dy, data$Hr, minutes,
                             0, tz = "UTC")
  
  data <- subset(data,as.Date(Timing) >= startDate & as.Date(Timing) <= endDate)
  
  data <- merge(data,location,by = "Code")

  data$Location <- droplevels(data$Location)
  data$Type <- droplevels(data$Type)
  data$Units <- droplevels(data$Units)

  data <- merge(data,status,by="Status")
  
  data$Status <- data$Status_Description
  data$Status_Description <- NULL
  
  data$Status <- factor(as.character(data$Status), levels = c("Reasonable","Questionable","Erroneous","Surrogate","Missing"))

  data <- subset(data,select = c("Code","Location","Type","Units","Timing","Mean","Min","Max","Status","Surrogate"))
  
  if (status_arg %in% c("reasonable","questionable")) {
    bol <- data$Status == "Erroneous"
    if (status_arg == "reasonable")
      bol <- bol | data$Status == "Questionable"
    is.na(data$Mean[bol]) <- T
    is.na(data$Min[bol]) <- T
    is.na(data$Max[bol]) <- T
  }
  
  surrogate_data <- data[data$Surrogate != "no",]
  data <- data[data$Surrogate == "no",]
  if(nrow(data) == 0)
    stop("all the data is surrogate")
  
   if (add_spaces && (period %in% c("hourly","daily","annual"))) {
     by <- switch(period,
                  "annual" = "year",
                  "daily" = "day",
                  "hourly" = "hour")

     timing <- seq(from = as.POSIXct(as.character(startDate),tz="UTC"), to = as.POSIXct(as.character(endDate),tz="UTC"), by = by)
     timing <- data.frame(Timing = timing)
     
     timing <- merge(unique(subset(data, select = c("Code","Location","Type","Units"))),timing)
     
     data <- merge(timing,data, all.x = T)
     
     data$Status[is.na(data$Status)] <- "Missing"
     data$Surrogate[is.na(data$Surrogate)] <- "no" 
   }
  
  surrogate_data <- na.omit(surrogate_data)
  
  if(surrogate != "none" && nrow(surrogate_data) > 0) {

    surrogate_data$Status <- "Surrogate"
  
    if (surrogate != "duplicate") {
      if(any(duplicated(subset(surrogate_data,select=c("Code","Timing"))))) {
        
        get_sur <- function (d) {
          if(nrow(d) == 1)
            return (d)
          
          d$Mean <- mean(d$Mean)
          d$Min <- min(d$Min)
          d$Max <- max(d$Max)
          d$Surrogate <- paste(d$Surrogate,collapse = " + ")
          
          return(d[1,,drop=FALSE])
        }
        Code <- Timing <- NULL
        surrogate_data <- ddply(surrogate_data,.(Code,Timing), get_sur)
      }

      mer <- merge(subset(data,select=c("Code","Timing","Status")),
                   subset(surrogate_data,select=c("Code","Timing")))
      Status <- NULL      
      if (surrogate == "questionable") {
        mer <- subset(mer,Status %in% c("Questionable","Erroneous","Missing"))
      } else if (surrogate == "erroneous") {
        mer <- subset(mer,Status %in% c("Erroneous","Missing"))
      } else if (surrogate == "missing") {
        mer <- subset(mer,Status %in% c("Missing"))
      }
      id <- paste(mer$Code,mer$Timing)
      if(length(id) > 0)
        data <- data[!paste(data$Code,data$Timing) %in% id,]
      
      surrogate_data <- surrogate_data[
        !paste(surrogate_data$Code,surrogate_data$Timing) %in%
          paste(data$Code,data$Timing),]
    }
    data <- rbind(data,surrogate_data)
  }
  data$Status <- factor(as.character(data$Status), levels = c("Reasonable","Questionable","Erroneous","Surrogate","Missing"))
  data$Surrogate <- factor(data$Surrogate)
  data$Surrogate <- relevel(data$Surrogate,"no")
  data <- data[order(data$Code,data$Timing,data$Surrogate,data$Status),]
  return (data)
}
