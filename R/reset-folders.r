
#' @export
reset_folders <- function () {  
  set_rdata_folder (dir = '../rdata')
  set_analyses_folder (dir = '../analyses')
  set_plots_folder (dir = '../plots')
  set_tables_folder (dir = '../tables')
  
  invisible ()
}
