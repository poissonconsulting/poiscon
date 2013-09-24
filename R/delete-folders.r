
#' @export
delete_folders <- function () {
  
  yesno <- function(question) {
    yeses <- c("Yes", "Definitely", "For sure", "Yup", "Yeah")
    nos <- c("No way", "Not yet", "I forgot", "No", "Nope")
    
    cat(question)
    qs <- c(sample(yeses, 1), sample(nos, 2))
    rand <- sample(length(qs))
    
    menu(qs[rand]) != which(rand == 1)
  }
  
  reset_dirs()
  
  folders <- c(getOption ("folders.rdata_directory"),
               getOption ("folders.analyses_directory"),
               getOption ("folders.plots_directory"),
               getOption ("folders.tables_directory"))
    
  folders <- paste(getwd(),folders,sep="/")

  folders <- str_replace(folders,"/R/..","")
  
  dirs <- list.dirs(folders,full.names = TRUE, recursive = TRUE)
  files <- list.files(folders,full.names = TRUE, recursive = TRUE)
  
  files <- c(dirs, files)
  
  if(length(files) == 0) {
    cat("There are no files to delete")
    return (invisible(1))
  }
  
  cat(files)
  
  if (yesno("\n\nAre you sure you want to delete all of the above files?"))
    return (invisible(1))

  return (unlink(files, recursive = TRUE))
}
