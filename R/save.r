#' @title Save rdata
#'
#' @description 
#' Saves object as .rdata files in current rdata folder.
#' 
#' @param object the object to save.
#' @param name a character scalar of the name of the file.
#' @return Saves object as .rdata files in current rdata folder.
#' @export
save_rdata<-function (object, name = "data") {
  
  file <- paste0(get_rdata_folder(), "/", name, ".rds")
  
  saveRDS(object, file)
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
  assert_that(is.jags_analysis(object))
  
  file <- paste0(get_analyses_folder(), '/', name, '.rds')
  
  saveRDS(object, file)
}

#' @title Save plot
#'
#' @description 
#' Saves current plot as .png file in current plots folder.
#' 
#' @param name a character scalar of the name of the object.
#' @param type a character scalar of the type of folder to save in.
#' @param save_table a logical scalar indicating whether to save the plot data.frame 
#' as a csv file or a integer scalar indicating the maximum number of rows to save the
#' csv file for.
#' @param report logical scalar indicating whether the plot should be displayed
#' to an analysis report.
#' @param caption character scalar of caption else NULL.
#' #' @param width a numeric scalar indicating the percent of the page width (or the 
#' number of plots per page width - up to 10)
#' @param height a numeric scalar indicating the percent of the page height - by default
#' the same percent as the width (or the 
#' number of plots per page width - up to 10).
#' @param dpi integer scalar of dots per inch. If NULL (the default) then
#' given by option poiscon.dpi which by default is 320
#' @return Saves current plot as .png file in current plots folder.
#' @export
save_plot <- function (name = "plot", type = "figures",
                     save_table = 100, report = TRUE, caption = NULL, 
                     width = NULL, height = NULL,dpi = NULL) {
  
  assert_that(is.string(name))
  assert_that(is.string(type))
  assert_that((is.flag(save_table) || is.count(save_table)) && noNA(save_table))
  assert_that(is.flag(report) && noNA(report))
  assert_that(is.null(caption) || is.string(caption))
  assert_that(is.null(dpi) || (is.count(dpi) && noNA(dpi)))
  
  if (type != "figures")
    report <- FALSE 

  if (is.null(width))
    width <- getOption("poiscon.gwindow.width", 100)
  if (is.null(height))
    height <- getOption("poiscon.gwindow.height", width)
  
  if(width <= 10) {
    width <- 100 / width
    height <- 100 / height
  }
  
  if(is.null(dpi))
    dpi <- getOption("poiscon.dpi", 320)    
   
  gplot <- gplot(ggplot = last_plot(), width = width, height = height, 
              dpi = dpi, report = report, caption = caption)
  
  filename <- paste0(get_plots_folder(type=type), "/", name)
  
  saveRDS(gplot, file = paste0(filename, ".rds"))
  
  data <- dataset(gplot)
  
  if (is.numeric(save_table))
    save_table <- nrow(data) < save_table
  
  if (save_table)
    write.csv(data, file = paste0(filename, ".csv"),row.names = FALSE)
  
  page_width <- getOption("poiscon.page_width", 6)
  
  width <- width / 100 * page_width
  height <- height / 100 * page_width

  ggsave(paste0(filename, ".png"), width = width, height = height, dpi = dpi)
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

  write.csv(object, filename, row.names = row.names)
}
