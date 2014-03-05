.onAttach <- function(libname, pkgname) {
  
  reset_folders()
  
#  set_fonts()
  
  theme_set(theme_Poisson())
  palette(palette_Poisson())
  
  invisible()
}
