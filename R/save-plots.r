#' @export
save_plots<- function (object, ...) {
  UseMethod("save_plots", object)
}

#' @S3method save_plots jags_analysis
#' @method save_plots jags_analysis
#' @export
save_plots.jags_analysis <- function (object) {
  file <- paste0(get_plots_folder(type = 'analyses'), '/trace.pdf')
  pdf(file=file,width=8.5,height=11)
  plot(object)
  dev.off()
}

#' @S3method save_plots data.frame
#' @method save_plots data.frame
#' @export
save_plots.data.frame <- function (object, name = "data") {
  file <- paste0(get_plots_folder(type = 'data'),'/',name,'.pdf')
  pdf(file=file,width=6,height=6)
  
  names <- colnames(object)
    
  for (i in 1:(length(names)-1)) {
    for (j in (i+1):length(names)) {
      object$x <- object[[names[i]]]
      object$y <- object[[names[j]]]
            
      width <- 0
      height <- 0
      if (is.factor(object$x)) {
        width <- 0.1
      }
      if (is.factor(object$y)) {
        height <- 0.1
      }        

      gp <- ggplot(data = object, aes(x = x, y = y))
      gp <- gp + geom_point(alpha=1/2,
                            position=position_jitter(width=width, height=height))      
      gp <- gp + xlab(names[i])
      gp <- gp + ylab(names[j])
      gp <- gp + theme(text = element_text(family = "mono"))
      
      print(gp)
    }
  }
  dev.off()
  invisible(data)
}
