
#' @title Plot residuals
#'
#' @description 
#'Save residual plots from a jaggernaut jags_analysis object to a pdf file.
#' 
#' @param object a jags_analysis object.
#' @param model a count or flag specifying the model to select. 
#' @param parm_residual a character element naming the residuals parameter.
#' @param parm_fitted a character element naming the fitted parameter.
#' @param name a character scalar naming the file.
#' @param derived_code a character element defining a block in the JAGS dialect of 
#' the BUGS language that defines one or more derived parameters for each row of data. 
#' If NULL derived_code is as defined by the JAGS model for which the JAGS analysis was performed. 
#' @param random_effects a named list which specifies which parameters to treat 
#' as random variables. If NULL random is as defined by the JAGS model for which the JAGS analysis was performed. 
#' @param level a numeric scalar specifying the significance level or a character
#' scalar specifying which mode the level should be taken from. By default the
#' level is as currently specified by \code{opts_jagr} in the global options.
#' @param data the dataset for which to calculate the residuals. By default data is NULL as the residuals are typically calculated on the original dataset.
#' @param ... further arguments passed to or from other methods.
#' @return Save plots to a pdf file.
#' @importFrom ggplot2 ggplot aes_string geom_histogram geom_vline geom_hline geom_pointrange xlab expand_limits ylab
#' @export
plot_residuals <- function(object, model = 1, parm_residual = "residual", 
  parm_fitted = "prediction", name = "residuals", derived_code = NULL, random_effects = NULL, 
  level = "current", data = NULL, ...) {
  
  res <- residuals(object, model = model, parm = parm_residual, derived_code = derived_code, 
    random_effects = random_effects, level = level, data = data, ...)
  
  fit <- fitted(object, model = model, parm = parm_fitted, derived_code = derived_code, 
    random_effects = random_effects, level = level, data = data, ...)
  
  res$fitted <- fit$estimate
  
  names <- colnames(res)
  names <- names[!names %in% c("fitted", "estimate", "lower", "upper", "error", 
    "significance")]
  
  names <- c("fitted", names)
  
  file <- paste0(get_plots_folder(type = "analyses"), "/", name, ".pdf")
  pdf(file = file, width = 6, height = 6)
  
  range <- max(res$estimate, na.rm = T) - min(res$estimate, na.rm = T)
  
  gp <- ggplot(data = res, aes_string(x = "estimate"))
  gp <- gp + geom_histogram(binwidth = range/30, color = "white")
  gp <- gp + geom_vline(xintercept = 0, color = "grey50")
  gp <- gp + xlab("residual")
  gp <- gp + expand_limits(x = 0)
  gp <- gp + theme(text = element_text(family = "mono"))
  
  print(gp)
  
  for (name in names) {
    res$x <- res[[name]]
    
    width <- 0
    if (is.factor(res$x)) {
      width <- 0.1
    }
    
    gp <- ggplot(data = res, aes_string(x = "x", y = "estimate"))
    gp <- gp + geom_hline(yintercept = 0, color = "grey50")
    gp <- gp + geom_pointrange(aes_string(ymin = "lower", ymax = "upper"), alpha = 1/3, 
      position = position_jitter(width = width, height = 0))
    gp <- gp + xlab(name)
    gp <- gp + ylab("residual")
    gp <- gp + expand_limits(y = 0)
    gp <- gp + theme(text = element_text(family = "Courier"))
    
    print(gp)
  }
  dev.off()
} 
