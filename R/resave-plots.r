
#' @title Resave plots
#'
#' @description
#' Can be used to resave plots with different theme and/or if in windows to 
#' ensure correct height and width when inserted into Microsoft Word document#' 
#' @param type a character element indicating the type of plots to redraw by default type = "figures"
#' @return invisible vector of files resaved
#' @export
resave_plots <- function (type = "figures") {

  if (.Platform$OS.type == "unix") {
    windows <- function (width = 7, height = 7,...) {
      quartz (width=width, height=height, ...)
    }
  }
  
  dir <- get_plots_folder(type = type)
  files <- list.files(dir, recursive = TRUE)
  files <- files[substr(files,nchar(files)-3,nchar(files)) == '.rds']
  files <- substr(files,1,nchar(files)-4)
  
  for (file in files) {
    gp <- readRDS(file=paste0(dir,"/",file,".rds"))
    width <- gp$width
    height <- gp$height
    dpi <- gp$dpi
    gp <- gp$ggplot
    
    windows (width = width, height = height)
    
    err <- try(print(gp))
    
    if(class(err) != "try-error") {
      ggsave(paste0(dir,"/",file,".png"), width = width, height = height, dpi = dpi)
      graphics.off()  
    } else {
      print (err)
    } 
  }
  invisible (files)
}
