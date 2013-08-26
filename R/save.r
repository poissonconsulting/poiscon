
#' @export
save_rdata<-function (object, name = 'data') {
  file <- paste0(get_rdata_folder(), '/', name, '.rds')
  return (saveRDS(object, file))
}

#' @export
save_analysis<-function (object, name = 'analysis') {
  file <- paste0(get_analyses_folder(), '/', name, '.rds')
  return (saveRDS(object, file))
}

#' @export
save_plot<-function (name='plot', type='figures', dwidth=NULL, dheight=NULL, slide=NULL, saveTable = TRUE) {    

  slide <- type == 'slides'
  
  if (is.null(dwidth))
    dwidth <- getOption("gwindow.dwidth",1)
  if (is.null(dheight))
    dheight <- getOption("gwindow.dheight",1)
  if (is.null(slide))
    slide <- getOption("gwindow.slide",FALSE)
  
  if (slide) {
    width <- getOption("gwindow.width_slide",10) / dwidth
    height <- getOption("gwindow.height_slide",7.5) / dheight
    dpi <- getOption("gwindow.dpi_slide",96)
  } else {
    width <- getOption("gwindow.width",6) / dwidth
    height <- getOption("gwindow.height",6) / dheight
    dpi <- getOption("gwindow.dpi",300)    
  }
  
  filename <- paste0(get_plots_folder(type=type), '/', name)
      
  if (saveTable)
    write.csv(last_plot()$data,file=paste0(filename,'.csv'),row.names=F)
  
  gp <- last_plot()
  gp <- list (ggplot = gp, width = width, height = height, dpi = dpi)
  class(gp) <- 'gp'
  
  saveRDS(gp,file=paste0(filename,'.rds'))
  
  return (ggsave(paste0(filename, '.png'), width = width, height = height, dpi = dpi))
}

#' @export
save_table<-function (object, name='table', type='results', row.names = FALSE) {
  object <- as.data.frame(object)
  filename <- paste0(get_tables_folder(type=type), '/', name,'.csv')
  return (write.csv(object, filename, row.names = row.names))
}
