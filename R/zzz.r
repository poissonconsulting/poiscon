.onAttach <- function(libname, pkgname) {
  
  reset_folders()
  
  if (.Platform$OS.type == "unix") {
    if (Sys.info()["sysname"] == "Darwin") {
      grDevices::quartzFonts(sans = quartzFont(rep("Arial", 4)))
    } else {
      stop("need to set font to Arial for linux operating systems")
    }
  } else {
    grDevices::windowsFonts(Arial = windowsFont("Arial"))
  }
  
  theme_set(theme_Poisson())
  grDevices::palette(palette_Poisson())
  
  invisible()
}
