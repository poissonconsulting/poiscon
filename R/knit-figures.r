#' @title Knit figures
#'
#' @description
#' Takes all plots in figures folder and embeds in markdown
#' format using html.
#' 
#' @param replacement an optional named character vector which specifies replacement
#' names for directories as well as the order to print the estimates.
#' @return Embeds figures in markdown format.
#' @export
knit_figures <- function (replacement = NULL) {

  assert_that(is.null(replacement) || 
                (is.character(replacement) && is_named(replacement)))
  
  reset_folders()
  
  dir <- get_plots_folder(type = "figures")
  files <- list.files(dir, pattern = "[.]rds", recursive = TRUE)
  files <- substr(files,1,nchar(files)-4)
  
  
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
    
    gp <- readRDS(file=paste0(dir,"/",file,".rds"))
    width <- gp$width / 6 * 100
    padding_left <- 100 - width / 2
    
    text <- strsplit(file,"/",fixed = T)[[1]]
    text <- text[-length(text)]
    title <- paste0(text, collapse = " - ")
    
    for (i in seq_along(replacement)) {
      title <- str_replace_all(title, names(replacement)[i], replacement[i])
    }
    
    title <- gsub("(^|[[:space:]|-])([[:alpha:]])", "\\1\\U\\2", title,
                  perl=TRUE)
    
    if(is.null(previous_title) || title != previous_title) {
      cat("\n<div style=\"text-align: left\">\n")
      cat(c("\n###"," ",title,"\n"))
      cat("</div\">\n")
    }
    previous_title <- title
    
    cat("\n<div style=\"text-align: left\" title = \"",file,"\">\n")
    cat(paste0("<img alt = ",file," src = \"",dir,"/",file,".png\" width = \"",width,"%  >\n"))
    cat("</div\">\n")    
  }
  return (invisible())
}
