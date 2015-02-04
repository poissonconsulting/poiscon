#' Perform Analyses
#' 
#' Performs batch jags analyses. It goes through the folders defined by \code{...}
#' and pulls out the data of name \code{name} (by default \code{"data"} and then fits
#' the models defined in \code{models}. Note \code{beep = TRUE} has caused
#' crashed with RStudio on windows.
#' 
#' @param models A jags_model object.
#' @param ... A series of character vectors or a list of character vectors of 
#' folders to cycle through.
#' @param niters A count of the number of iterations to perform.
#' @param mode A string of the opts_jagr mode.
#' @param name A string of the name of the data file.
#' @param beep A flag indicating whether to beep on completion.
#' @export
perform_analyses <- function(models, ..., niters = 10^3, mode = "current", 
                             name = "data", 
                             beep = getOption("poiscon.beep", default = FALSE)) {
  
  assert_that(is.jags_model(models))
  assert_that(is.count(niters))
  assert_that(is.string(mode))
  assert_that(is.string(name))
  assert_that(is.flag(beep) && noNA(beep))
  
  opts <- opts_jagr()
  on.exit(opts_jagr(opts))
  opts_jagr(mode = mode)
  
  args <- list(...)
  nargs <- length(args)
  
  analysis <- function(models, name, niters) {
    data <- load_rdata(name)
    
    analysis <- jags_analysis(models, data = data, niters = niters)
    
    save_analysis(analysis)
    print(summary(analysis))
    save_tables(analysis)
    save_plots(analysis)
  }
  
  if (nargs == 0) {
    analysis(models = models, name = name, niters = niters)
  } else {
    folders <- t(expand.grid(...))
    for (i in 1:ncol(folders)) {
      print(set_folders(as.character(as.list(unlist(folders[, i])))))
      analysis(models = models, name = name, niters = niters)
    }
  }
  if(beep)
    beepr::beep(10)
  invisible(TRUE)
}

#' Plots Residuals
#' 
#' Plots
#' 
#' @param ... character vectors of folders to cycle through
#' @export
plot_residuals_analyses <- function(...) {
  args <- list(...)
  nargs <- length(args)
  
  plot_residuals_analysis <- function() {
    analysis <- load_analysis()
    plot_residuals(analysis)
  }
  
  if (nargs == 0) {
    plot_residuals_analysis()
  } else {
    folders <- t(expand.grid(...))
    for (i in 1:ncol(folders)) {
      print(set_folders(as.character(as.list(unlist(folders[, i])))))
      plot_residuals_analysis()
    }
  }
  beepr::beep(10)
  invisible(TRUE)
}
