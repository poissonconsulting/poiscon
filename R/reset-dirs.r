
#' @export
reset_folders <- function (share = FALSE) {
  
  warning("reset_folders deprecated by reset_dirs")

  return (reset_dirs(share = share))
}

#' @title Reset poiscon directories
#'
#' @description
#' Resets poiscon directories to the default values. The directories are 
#' used for saving plots, tables, rdata and analyses.
#' 
#' @param share a logical element indicating whether or not the directories
#'  should be nested in the work directory (share = TRUE) or saved a level
#'   higher in the directory structure (share = FALSE) the default
#' @return NULL
#' @export
reset_dirs <- function (share = FALSE) {

  if(!inherits(share, "logical"))
    stop("share must be class logical")
  
  if(length(share) != 1)
    stop("share must be a logical vector of length one")
  
  if(is.na(share) != 1)
    stop("share must be TRUE or FALSE")
  
  if(share) {
    set_rdata_folder (dir = 'rdata')
    set_analyses_folder (dir = 'analyses')
    set_plots_folder (dir = 'plots')
    set_tables_folder (dir = 'tables')
  } else {
    set_rdata_folder (dir = '../rdata')
    set_analyses_folder (dir = '../analyses')
    set_plots_folder (dir = '../plots')
    set_tables_folder (dir = '../tables')
  }
  
  invisible ()
}
