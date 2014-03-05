#' @title Sets fonts
#'
#' @description
#' Sets graphics fonts to Arial for OSX and windows
#' 
#' @return deck to web
#' @export
set_fonts <- function () {
if (.Platform$OS.type == "unix") {
    if (Sys.info()["sysname"] == "Darwin") {
      quartzFonts(sans = quartzFont(rep("Arial", 4)))
    } else {
      stop("need to set font to Arial for linux operating systems")
    }
  } else {
    windowsFonts(Arial = windowsFont("Arial"))
  }
  invisible(TRUE)
}