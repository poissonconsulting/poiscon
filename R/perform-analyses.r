#' @title Perform analyses
#'
#' @description
#' Run set of analyses
#' 
#' @param models a jags_model to use for the analyses
#' @param ... optional character vectors specifying the directories 
#' to pull the data from (and to save to save the analyses to)  
#' @param niters a integer scalar of the number of iterations
#' @param name a character scalar of the name of the data
#' @return Performs analyses using jaggernaut
#' @export
perform_analyses <- function (models, ..., niters = 10^3, name = "data") {
  
  assert_that(is.jags_model(models))
  assert_that(is.count(niters))
  assert_that(is.string(name))
  args <- list(...)
  nargs <- length(args)
  
  analysis <- function (models, name, niters) {
    data <- load_rdata(name)
    
    analysis <- jags_analysis(models, data = data, niters = niters)
    
    save_analysis(analysis)
    
    print(summary(analysis))
    
    save_tables(analysis)
    
    if (opts_jagr("mode") != "debug") {
      save_plots(analysis)
      dc <- derived_code(analysis)
      if (!is.null(dc)) {
        if(length(grep("residual[/[]", dc)) & length(grep("prediction[/[]", dc))) { 
          plot_residuals(analysis)
        }
      }  
    } else if (!is.null(derived_code(analysis))) {
      data <- dataset(analysis)
      if(is.data.frame(data)) {
        newdata <- data[1,,drop = FALSE]
        derived_code <- derived_code(analysis)
        if (!is.list(derived_code))
          derived_code <- list(derived_code)
        for(i in 1:length(derived_code)) {
          dc <- derived_code[[i]]
          if (length(grep("prediction[/[]",dc)))
            predict(analysis, model_number = i, newdata = newdata, parm = "prediction")
          if (length(grep("residual[/[]",dc)))
            predict(analysis, model_number = i, newdata = newdata, parm = "residual")
        }
      }
    }
  }
  
  if (nargs == 0) {
    analysis(models = models, name = name, niters = niters)
  } else {
    folders <- t(expand.grid(...))
    for (i in 1:ncol(folders)) {
      cat("\n\n")
      cat(as.character(folders[,i]))
      cat("\n\n")
      set_folders(as.character(folders[,i]))
      
      analysis(models = models, name = name, niters = niters)      
    }
  }
  ping()
  return (invisible())
}
