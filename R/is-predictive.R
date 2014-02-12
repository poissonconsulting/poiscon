#' @title Is Predictive
#'
#' @description
#' Tests whether a prediction data.frame is informative i.e.,
#' has variation in estimate and/or lower/upper.
#' 
#' @param x data.frame of predictions to test informative
#' @return logical scalar indicating whether informative
#' @export
is_predictive <- function (x) {
  assert_that(is.data.frame(x))
  assert_that(all(c("estimate","lower","upper") %in% colnames(x)))

  if(nrow(x) == 0)
    return (FALSE)
  
  if(!all(x$estimate == x$estimate[1]))    
    return (TRUE)
  
  if(!all(x$lower == x$lower[1]))    
    return (TRUE)

  if(!all(x$upper == x$upper[1]))    
    return (TRUE)
  
  FALSE
}
