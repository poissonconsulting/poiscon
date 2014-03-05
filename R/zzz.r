.onAttach <- function(libname, pkgname) {
  
  reset_folders()
  
#   if (.Platform$OS.type == "unix") {
#     if (Sys.info()["sysname"] == "Darwin") {
#       grDevices::quartzFonts(sans = grDevices::quartzFont(rep("Arial", 4)))
#     } else {
#       stop("need to set font to Arial for linux operating systems")
#     }
#   } else {
#     grDevices::windowsFonts(Arial = grDevices::windowsFont("Arial"))
#   }
#   grDevices::palette(palette_Poisson())
  
  theme_set(theme_Poisson())

  
  invisible()
}
