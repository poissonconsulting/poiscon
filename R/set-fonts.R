set_fonts <- function () {
  require(grDevices)

  if (.Platform$OS.type == "unix") {
    if (Sys.info()["sysname"] == "Darwin") {
      quartzFonts(sans = quartzFont(rep("Arial", 4)))
    } else {
      stop("need to set font to Arial for linux operating systems")
    }
  } else {
    windowsFonts(Arial = windowsFont("Arial"))
  }
  graphics.off()
}
