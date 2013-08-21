
#' Create new plotting window
#'
#' Creates a new window for plotting
#'
#' @param dwidth the proportion of the page width
#' @param dheight the proprotion of the page height
#' @param slide whether or not powerpoint slide (TRUE) versus plot
#' @export
gwindow <- function (dwidth = 1, dheight = 1, slide = FALSE) {  
  
  if (.Platform$OS.type == "unix") {
    windows <- function (width = 7, height = 7,...) {
      quartz (width=width, height=height, ...)
    }
  }

  options(gwindow.dwidth = dwidth)
  options(gwindow.dheight = dheight)
  options(gwindow.slide = slide)
  
  if (slide)
    return (windows (width = getOption("gwindow.width_slide",10) / dwidth, height = getOption("gwindow.height_slide",7.5) / dheight))    
  
  return (windows (width = getOption("gwindow.width",6) / dwidth, height= getOption("gwindow.height",6) / dheight))    
}
