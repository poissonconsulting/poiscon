
#' @title Save plots
#'
#' @description
#' Plots object to a pdf file
#'
#' @param object object to plot
#' @param ... further arguments passed to or from other methods.
#' @return Save plot as a pdf file.
#' @export
save_plots <- function(object, ...) {
  UseMethod("save_plots", object)
}

#' @title Save plots
#'
#' @description
#' Plots jags_analysis object to a pdf file
#'
#' @param object jags_analysis object to plot
#' @param model a number or string of the model to plot
#' @param ... further arguments passed to or from other methods.
#' @return Saves jags_analysis as a pdf file.
#' @method save_plots jags_analysis
#' @export
save_plots.jags_analysis <- function(object, model = 1, ...) {
  file <- paste0(get_plots_folder(type = "analyses"), "/trace.pdf")
  grDevices::pdf(file = file, width = 8.5, height = 11)
  plot(object, model = model)
  grDevices::dev.off()
}

#' @title Save plots
#'
#' @description
#' Plots data.frame object to a pdf file
#'
#' @param object data.frame object to plot
#' @param name a character scalar of the file name.
#' @param ... further arguments passed to or from other methods.
#' @return Saves data.frame as a pdf file.
#' @method save_plots data.frame
#' @importFrom ggplot2 ggplot aes_string geom_point xlab position_jitter ylab
#' @export
save_plots.data.frame <- function(object, name = "data", ...) {
  file <- paste0(get_plots_folder(type = "data"), "/", name, ".pdf")
  pdf(file = file, width = 6, height = 6)

  names <- colnames(object)

  for (i in 1:(length(names) - 1)) {
    for (j in (i + 1):length(names)) {
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

      gp <- ggplot(data = object, aes_string(x = "x", y = "y"))
      gp <- gp + geom_point(alpha = 1/2, position = position_jitter(width = width,
        height = height))
      gp <- gp + xlab(names[i])
      gp <- gp + ylab(names[j])
      gp <- gp + theme(text = element_text(family = "mono"))

      print(gp)
    }
  }
  grDevices::dev.off()
  return(invisible(data))
}
