.onAttach <- function(libname, pkgname) {
  
  suppressPackageStartupMessages(require(grDevices))
  reset_folders()
  
  if (.Platform$OS.type == "unix") {
    if (Sys.info()["sysname"] == "Darwin") {
      quartzFonts(sans = quartzFont(rep("Arial", 4)))
    } else {
      stop("need to set font to Arial for linux operating systems")
    }
  } else {
    windowsFonts(Arial = windowsFont("Arial"))
  }
  
  theme_set(theme_Poisson())
  palette(palette_Poisson())
  
  invisible()
}
