
#' @export
upload_bch_data <- function (data, access_file = "../../../bchydro_data_12/data/based/upload/ColDuncEnv.accdb")
{
  if (!inherits(data,"data.frame"))
    stop("data must be class data.frame")
  
  if (!all(c("Timing","Variable","Level") %in% colnames(data)))
    stop("data must have columns Timing, Variable and Level")
  
  if(.Platform$OS.type != "windows")
    stop("only available for windows")
  
  data$Yr <- year(data$Timing)
  data$Mon <- month(data$Timing)
  data$Dy <- day(data$Timing)
  data$Hr <- hour(data$Timing)
  data$Min <- minute(data$Timing)
  sec <- second(data$Timing)
  
  if(any(sec > 0))
    stop("seconds must be zero")
    
  data$Variable <- as.character(data$Variable)
  
  if(is.null(data$Surrogate))
    data$Surrogate <- "no"
  
  if(is.null(data$Status_BCH)) 
    data$Status_BCH <- NA
  
  data$Status <- 0
  data$Comments <- NA
  
  data$Status[!is.na(data$Status_BCH) & data$Status_BCH %in% c(55,200)] <- 1
  data$Comments[!is.na(data$Status_BCH) & data$Status_BCH == 55] <- "Estimated"  
  data$Comments[!is.na(data$Status_BCH) & data$Status_BCH == 200] <- "Raw"  
  
  data <- subset(data,select = c(Variable,Yr,Mon,Dy,Hr,Min,Level,Surrogate,Status,Comments))
  
  db <- odbcConnectAccess2007(access_file)

  on.exit(odbcCloseAll())
    
  if(!nrow(sqlTables(db, tableName = "Location")))
    stop("Location table is not defined")
  
  location <- sqlFetch(db, sqtable = "Location", colnames = FALSE, rownames = TRUE)
    
  location$Variable <- as.character(location$Variable)
  
  variable <- unique(data$Variable[!data$Variable %in% location$Variable])
    
  if(length(variable))
    warning(c("the following variables are unrecognized:",variable))
  
  data <- merge(location,data,by="Variable")
  
  data <- subset(data,select = c(Code,Yr,Mon,Dy,Hr,Min,Level,Surrogate,Status,Comments))

  if (nrow(data)) {
    if (nrow(sqlTables(db, tableName = "Uploaded"))) {
      table <- sqlFetch(db, sqtable = "Uploaded", colnames = FALSE, rownames = TRUE)
      
      if(!identical(colnames(table),c("Code","Yr","Mon","Dy","Hr","Min","Level","Surrogate","Status","Comments")))
        stop("incompatible upload table")
    }  
    sqlSave(db, dat = data, tablename = "Upload", append = TRUE, rownames = FALSE)
  }
  message(paste0(nrow(data)," rows imported"))
  invisible(1)
}
