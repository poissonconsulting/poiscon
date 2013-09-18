
#' @export
plot_residuals <- function (object, model_number = 1, parm_residual = "residual", parm_fitted = "prediction",  name = "residuals", derived_code = NULL, random_effects = NULL, level = "current", ...) {
    
  data <- residuals (object, model_number = model_number, parm = parm_residual,
                    derived_code = derived_code, random_effects = random_effects,
                    level = level, ...)
  fit <- fitted (object, model_number = model_number, parm = parm_fitted,
                 derived_code = derived_code, random_effects = random_effects,
                 level = level, ...)
  
  data$fitted <- fit$estimate
    
  names <- colnames(data)
  names <- names[!names %in% c("fitted","estimate","lower","upper","error","significance")]
  
  names <- c("fitted", names)
  
  file <- paste0(get_plots_folder(type = 'analyses'),'/',name,'.pdf')
  pdf(file=file,width=6,height=6)
  
  range <- max(data$estimate,na.rm = T) - min(data$estimate,na.rm = T)
  
  gp <- ggplot(data = data, aes(x = estimate))
  gp <- gp + geom_histogram(binwidth = range/30, color = "white")
  gp <- gp + geom_vline(xintercept = 0, color="grey50")
  gp <- gp + xlab("residual")
  gp <- gp + expand_limits(x = 0)
  gp <- gp + theme(text = element_text(family = "mono"))
  
  print(gp)
  
  for (name in names) {
    data$x <- data[[name]]
    
    width <- 0
    if (is.factor(data$x)) {
      width <- 0.1
    } 
    
    gp <- ggplot(data = data, aes(x = x, y = estimate))
    gp <- gp + geom_hline(yintercept = 0,color="grey50")
    gp <- gp + geom_pointrange(aes(ymin = lower, 
                                   ymax = upper),
                                    alpha=1/3,
                            position=position_jitter(width=width, height=0))
    gp <- gp + xlab(name)
    gp <- gp + ylab("residual")
    gp <- gp + expand_limits(y = 0)
    gp <- gp + theme(text = element_text(family = "Courier"))
    
    print(gp)
  }
  dev.off()
}
