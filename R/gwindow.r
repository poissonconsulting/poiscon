#' @title Plotting window
#'
#' @description
#' Creates a new platform-independent window for plotting
#'
#' @param width a numeric scalar indicating the percent of the page width (or the 
#' number of plots per page width - up to 10)
#' @param height a numeric scalar indicating the percent of the page height - by default
#' the same percent as the width (or the 
#' number of plots per page width - up to 10).
#' @return Creates a new window for plotting
#' @export
gwindow <- function (width = 100, height = NULL) {
  
  assert_that(is.numeric(width) && noNA(width))
  assert_that(is.null(height) || (is.numeric(height) && noNA(height)))
  
  if (.Platform$OS.type == "unix") {
    windows <- function (width = 7, height = 7,...) {
      quartz (width = width, height = height, ...)
    }
  }
  
  if(is.null(height))
    height <- width
  
  if(width <= 10) {
    width <- 100 / width
    height <- 100 / height
  }

  options(poiscon.gwindow.width = width)
  options(poiscon.gwindow.height = height)   
  
  page_width <- getOption("poiscon.page_width", 6)

  width <- page_width * width / 100
  height <- page_width * height / 100
  
  windows (width = width, height = height)    
}
