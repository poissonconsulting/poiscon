#' @title Resave plots
#'
#' @description
#' Can be used to resave plots with different theme and/or if in windows to 
#' ensure correct height and width when inserted into Microsoft Word document 
#' @param type a character element indicating the type of plots to redraw by default type = "figures"
#' @param dpi integer scalar of dots per inch. If NULL (the default) then
#' given by option poiscon.dpi which by default is 300.
#' @return invisible logical scalar indicating whether successful.
#' @export
resave_plots <- function (type = "figures", dpi = NULL) {
  
  assert_that(is.string(type))
  assert_that(is.null(dpi) || (is.count(dpi) && noNA(dpi)))
  

  if (.Platform$OS.type == "unix") {
    windows <- function (width = 7, height = 7,...) {
      quartz (width = width, height = height, ...)
    }
  }
  
  dir <- get_plots_folder(type = type)
  files <- list.files(dir, recursive = TRUE)
  files <- files[substr(files,nchar(files)-3,nchar(files)) == '.rds']
  files <- substr(files,1,nchar(files)-4)
  
  success <- TRUE
  
  for (file in files) {
    gp <- readRDS(file = paste0(dir, "/", file, ".rds"))
    
    width <- gp$width
    height <- gp$height
    gp <- gp$ggplot
    
    page_width <- getOption("poiscon.page_width", 6)
    
    width <- page_width * width / 100
    height <- page_width * height / 100
    
    windows (width = width, height = height)
    
    err <- try(print(gp))
    
    if(class(err) != "try-error") {
      ggsave(paste0(dir, "/", file, ".png"), width = width, height = height, 
             dpi = ifelse(is.null(dpi), gp$dpi, dpi))
      graphics.off()  
    } else {
      print (err)
      success <- FALSE
    } 
  }
  invisible(success)
}
