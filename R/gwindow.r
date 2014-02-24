open_window <- function (width, height) {
  fun <- switch(Sys.info()["sysname"],
         Windows = windows,
         Darwin = quartz,
         stop("gwindow not yet implemented for this operating system"))

  fun(width = width, height = height)
}

#' @title Open graphics window
#'
#' @description
#' Opens a new graphics window for plotting
#'
#' @param width a numeric scalar indicating the percent of the page width (or 
#' if 10 or less the width of the page in inches)
#' @param height a numeric scalar indicating the percent of the page height (or 
#' if 10 or less the width of the page in inches). By default height is
#' the same as the width.
#' @return a new graphics window for plotting
#' @seealso \code{\link{save_plot}}
#' @examples
#' \dontrun{
#' 
#' gwindow(50)
#' gwindow(3)
#' gwindow(50, 100)
#' gwindow(3, 6)
#' gwindow(3, 100)
#' }
#' @export
gwindow <- function (width = 100, height = width) {
  
  assert_that(is.number(width))
  assert_that(is.number(height))
  
  page_width <- getOption("poiscon.page_width", 6)
  
  if(width <= 10)
    width <- round(width / page_width * 100)
  
  if(height <= 10)
    height <- round(height / page_width * 100)

  options(poiscon.gwindow.width = width)
  options(poiscon.gwindow.height = height)   
  
  width <- page_width * width / 100
  height <- page_width * height / 100
  
  open_window (width = width, height = height)    
}
