
#' @title Plotting window
#'
#' @description
#' Creates a new platform-independent window for plotting
#'
#' @param dwidth a numeric scalar indicating the proportion of the page width
#' @param dheight a numeric scalar indicating the proprotion of the page height
#' @param slide a logical scalar indicating whether plot (the default) versus powerpoint slide.
#' @return Creates a new window for plotting
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
