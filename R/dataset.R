#' @title Get dataset
#'
#' @description
#' Gets the dataset from a \code{ggplot} object.  
#' 
#' @param object a \code{ggplot} object.
#' @param ... further arguments passed to or from other methods.
#' @return The dataset.
#' @method dataset ggplot
#' @export
dataset.ggplot <- function (object, ...) {
  object$data
}
  
dataset.gplot <- function (object, ...) {
  dataset(ggplot(object))
}
