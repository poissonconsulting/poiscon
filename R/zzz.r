#' @importFrom ggplot2 theme_set
.onLoad <- function(libname, pkgname) {
  
  reset_folders()
  
  set_colors()
  theme_set(theme_Poisson())
  
  invisible()
} 
