#' @title Save tables
#'
#' @description
#' Saves object to a tables file
#' 
#' @param object object to save tables
#' @param ... further arguments passed to or from other methods.
#' @return Save object as a tables.
#' @export
save_tables <- function(object, ...) {
  UseMethod("save_tables", object)
}
#' @title Save tables
#'
#' @description
#' Saves jags_analysis to tables
#' 
#' @param object jags_analysis object to tables
#' @param model_number a integer scalar of the model number
#' @param ... further arguments passed to or from other methods.
#' @return Save object as a tables.
#' @method save_tables jags_analysis
#' @export
save_tables.jags_analysis <- function(object, model_number = 1, ...) {
  
  object <- subset(object, model_number = model_number)
  
  convergence_all <- convergence(object, combine = FALSE)
  convergence <- convergence(object, combine = TRUE)
  coef <- coef(object)
  niters <- niters(object)
  
  save_table(convergence_all, "convergence", type = "analyses")
  
  save_table(coef, "estimates")
  
  table <- data.frame(Convergence = convergence, Iterations = niters)
  
  save_table(table, "convergence")
} 
