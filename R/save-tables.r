#' @export
save_tables<- function (object, ...) {
  UseMethod("save_tables", object)
}

#' @S3method save_tables jags_analysis
#' @method save_tables jags_analysis
#' @export
save_tables.jags_analysis <- function (object) {
  
  save_table(rhat(object, combine = FALSE),
             'convergence',type='analyses', row.names = T
             )
  
  save_table(coef(object),'estimates', row.names = T)
}
