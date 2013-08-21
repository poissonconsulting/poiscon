
#' @export
is_predictive <- function (x) {
  
  if(!is.data.frame(x))
    stop("x should be class data.frame")

  if(!"estimate" %in% colnames(x))
    stop("estimate should be a column in x")
  
  return (diff(range(x$estimate)) > 0)
}
