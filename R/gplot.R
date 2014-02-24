gplot <- function (ggplot, width, height, report, caption) {
  
  object <- list(ggplot = ggplot, width = width, height = height, 
                 report = report, caption = caption)
  
  class(object) <- "gplot"
  
  object
}

is.gplot <- function (object) {
  inherits(object, "gplot")
}

ggplot.gplot <- function (object, ...) {  
  object$ggplot
}
