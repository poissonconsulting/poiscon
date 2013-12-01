
#' @title Save rdata
#'
#' @description 
#' Saves object as .rdata files in current rdata folder.
#' 
#' @param object the object to save.
#' @param name a character scalar of the name of the file.
#' @return Saves object as .rdata files in current rdata folder.
#' @export
save_rdata<-function (object, name = 'data') {
  file <- paste0(get_rdata_folder(), '/', name, '.rds')
  return (saveRDS(object, file))
}

#' @title Save analysis
#'
#' @description 
#' Saves object as .rdata files in current analysis folder.
#' 
#' @param object the object to save.
#' @param name a character scalar of the name of the file.
#' @return Saves object as .rdata files in current analysis folder.
#' @export
save_analysis<-function (object, name = 'analysis') {
  file <- paste0(get_analyses_folder(), '/', name, '.rds')
  return (saveRDS(object, file))
}

#' @title Save plot
#'
#' @description 
#' Saves current plot as .png file in current plots folder.
#' 
#' @param name a character scalar of the name of the object.
#' @param type a character scalar of the type of folder to save in.
#' @param dwidth a numeric scalar indicating the proportion of the window width. By default NULL and taken from plot window.
#' @param dheight a numeric scalar indicating the proportion of the window height. By default NULL and taken from plot window.
#' @param slide a logical scalar or NULL indicating whether a slide window.
#' @param saveTable a logical scalar indicating whether to dave the plot data.frame 
#' as a csv file.#' 
#' @return Saves current plot as .png file in current plots folder.
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

#' @title Save table
#'
#' @description 
#' Saves object as .csv file in current table folder.
#' 
#' @param object the object to save.
#' @param name a character scalar of the name of the object.
#' @param type a character scalar of the type of folder to save in.
#' @param row.names a logical scalar of whether to include row names.
#' @return Saves object as .csv files in current table folder.
#' @export
save_table<-function (object, name='table', type='results', row.names = FALSE) {
  object <- as.data.frame(object)
  filename <- paste0(get_tables_folder(type=type), '/', name,'.csv')
  return (write.csv(object, filename, row.names = row.names))
}
