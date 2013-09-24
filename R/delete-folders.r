
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
  
  folders <- c(get_rdata_folder(),
               get_analyses_folder(),
               get_plots_folder(),
               get_tables_folder())
    
  folders <- paste(getwd(),folders,sep="/")

  folders <- str_replace(folders,"/R/..","")
  
  files <- list.files(folders,full.names = TRUE, recursive = TRUE)
  
  if(length(files) == 0) {
    cat("There are no files to delete")
    return (invisible(1))
  }
  
  cat(files)
  
  if (yesno("Are you sure you want to delete all of the above files?"))
    return (invisible(1))

  return (unlink(files))
}
