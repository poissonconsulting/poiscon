#' @export
save_tables<- function (object, ...) {
  UseMethod("save_tables", object)
}

#' @S3method save_tables jags_analysis
#' @method save_tables jags_analysis
#' @export
save_tables.jags_analysis <- function (object) {
  
  save_table(rhat(subset_jags(object, model_number = 0), combine = FALSE),
             'convergence',type='analyses', row.names = T)

  save_table(coef(subset_jags(object, model_number = 0)),'estimates', 
             row.names = T)
}
