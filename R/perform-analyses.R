#' Perform Analyses
#' 
#' Performs batch jags analyses
#' 
#' @param models jags_model objects
#' @param ... character vectors of folders to cycle through
#' @param niters count of number of iterations
#' @param name string of name of data file
#' @export
perform_analyses <- function(models, ..., niters = 10^3, name = "data") {
  
  assert_that(is.jags_model(models))
  assert_that(is.count(niters))
  assert_that(is.string(name))

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
      print(set_folders(as.character(folders[, i])))
      analysis(models = models, name = name, niters = niters)
    }
  }
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
      set_folders(as.character(folders[, i]))
      plot_residuals_analysis()
    }
  }
  beepr::beep(10)
  invisible(TRUE)
}
