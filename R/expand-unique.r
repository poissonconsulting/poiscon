
#' @export
expand_unique <- function (x, select = NULL, droplevels = FALSE) {
  stopifnot(is.data.frame(x))
  
  if(!is.null(select))
    x <- subset(x,select = select)
  
  data <- list()
  for (colname in colnames(x)) {
    vec <- x[[colname]]
    if (!is.factor(vec)) {
      data[colname] <- sort(unique(vec))
    } else {
      if (droplevels) 
        vec <- droplevels(vec)
      data[colname] <- levels(vec)
    }
  }
  data <- expand.grid(data)
  return (data)
}
