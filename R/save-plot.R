#' @title Save plot
#'
#' @description 
#' Saves current plot as a png file in current plots folder. The dpi is
#' given by option poiscon.dpi which by default is 320.
#' 
#' @param name a character scalar of the name of the object.
#' @param type a character scalar of the type of folder to save in.
#' @param report logical scalar indicating whether the plot should be displayed
#' to an analysis report.
#' @param caption character scalar of caption else NULL.
#' @param save_rows an integer scalar indicating the maximum of rows to save
#' from the plot data as a csv file.
#' @param width a numeric scalar indicating the percent of the page width (or 
#' if 10 or less the width of the page in inches). If NULL default the width is
#' taken from the width of the previous gwindow.
#' @param height a numeric scalar indicating the percent of the page height (or 
#' if 10 or less the width of the page in inches). If NULL default the height is
#' taken from the height of the previous gwindow.
#' @return Saves current plot as png file in current plots folder.
#' @seealso \code{\link{gwindow}}
#' @export
save_plot <- function (name = "plot", type = "figures",
                     report = TRUE, caption = NULL, save_rows = 100, 
                     width = NULL, height = NULL) {
  
  assert_that(is.string(name))
  assert_that(is.string(type))
  assert_that(is.flag(report) && noNA(report))
  assert_that(is.null(caption) || is.string(caption))
  assert_that(is.number(save_rows))
  assert_that(is.null(width) || is.numeric(width))
  assert_that(is.null(width) || is.numeric(height))
  
  if (type != "figures")
    report <- FALSE 

  page_width <- getOption("poiscon.page_width", 6)
  dpi <- getOption("poiscon.dpi", 320)    

  if (is.null(width))
    width <- getOption("poiscon.gwindow.width", 100)
  if (is.null(height))
    height <- getOption("poiscon.gwindow.height", width)
  
  if(width <= 10)
    width <- round(width / page_width * 100)
  
  if(height <= 10)
    height <- round(height / page_width * 100)
  
  file <- file.path(get_plots_folder(type=type), name)

  gplot <- gplot(ggplot = last_plot(), width = width, height = height, 
                 report = report, caption = caption)
    
  saveRDS(gplot, file = replace_ext(file, "rds"))
  
  data <- dataset(gplot)
  
  if(save_rows > 0) {
    write.csv(data[1:(min(save_rows, nrow(data))),], 
              file =  replace_ext(file, "csv"), row.names = FALSE)
  }
  
  width <- width / 100 * page_width
  height <- height / 100 * page_width

  ggsave(replace_ext(file, "png"), width = width, height = height, dpi = dpi)
}
