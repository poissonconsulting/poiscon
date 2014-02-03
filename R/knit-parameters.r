#' @title Knit parameters
#'
#' @description
#' Takes all parameter estimates in tables folder and prints in markdown
#' format using the knitr kable function.
#' 
#' @param replacement an optional named character vector which specifies replacement
#' names for directories as well as the order to print the estimates.
#' @return Prints parameter estimates in markdown format.
#' @export
knit_parameters <- function (replacement = NULL) {
  
  assert_that(is.null(replacement) || 
                (is.character(replacement) && is_named(replacement)))
  
  reset_folders()
  
  dir <- get_tables_folder(type = "results")
  files <- list.files(dir, pattern = "[.]csv", recursive = TRUE)
  files <- substr(files,1,nchar(files)-4)
  files <- files[substr(files,nchar(files)-8, nchar(files)) == "estimates"]
  
  if(!is.null(replacement)) {
    order <- format(1:length(replacement), width = 4)
    names(order) <- names(replacement)
    sort <- files
    
    for(i in seq_along(files)) {
      for (j in seq_along(order)) {
        sort[i] <- str_replace_all(sort[i], names(order)[j], order[j])
      }
    }
    files <- files[order(sort)]  
  }
  
  previous_title <- NULL
  
  for (file in files) {
        
    csv <- read.csv(file=paste0(dir,"/",file,".csv"))
    
    colnames(csv)[1] <- "Parameter"
    csv <- csv[csv$Parameter != "deviance",]
    csv <- csv[!is.na(csv$error),]
    csv <- csv[order(csv$Parameter),]
    colnames(csv) <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", colnames(csv),
                          perl=TRUE)
    colnames(csv) <- str_replace(colnames(csv), "Sd", "SD")
    
    text <- strsplit(file,"/",fixed = T)[[1]]
    text <- text[-length(text)]
    title <- paste0(text, collapse = " - ")
    
    for (i in seq_along(replacement)) {
      title <- str_replace_all(title, names(replacement)[i], replacement[i])
    }
    
    title <- gsub("(^|[[:space:]|-])([[:alpha:]])", "\\1\\U\\2", title,
                  perl=TRUE)
    
    if(is.null(previous_title) || title != previous_title) {
      cat(c("\n\n###"," ",title,"\n\n"))
    }
    previous_title <- title
    
    kable(csv, row.names = FALSE)
    
    file <- str_replace(file, "estimates", "rhat")
    
    if(file.exists(paste0(dir,"/",file,".csv"))) {
      csv <- read.csv(file=paste0(dir,"/",file,".csv"))
      cat("\n\n")
      kable(csv, row.names = FALSE)      
      cat("\n\n")
    }
  }
  return (invisible())
}
