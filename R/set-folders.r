
set_rdata_folder <- function (..., dir = getOption ("folders.rdata_directory")) {
  folder <- paste(..., sep = "/", collapse = "/")
  if (!length(folder))
    folder <- NULL
  
  options (folders.rdata_folder = folder)
  options (folders.rdata_directory = dir)
  invisible ()
}

set_analyses_folder<-function (..., dir = getOption ("folders.analyses_directory")) {
  folder <- paste(..., sep = "/", collapse = "/")
  if (!length(folder))
    folder <- NULL
  
  options (folders.analyses_folder = folder)
  options (folders.analyses_directory = dir)
  invisible ()
}

set_plots_folder<-function (..., dir = getOption ("folders.plots_directory")) {
  folder <- paste(..., sep = "/", collapse = "/")
  if (!length(folder))
    folder <- NULL
  
  options (folders.plots_folder = folder)
  options (folders.plots_directory = dir)
  invisible ()
}

set_tables_folder<-function (..., dir = getOption ("folders.tables_directory")) {
  folder <- paste(..., sep = "/", collapse = "/")
  if (!length(folder))
    folder <- NULL
  
  options (folders.tables_folder = folder)
  options (folders.tables_directory = dir)
  invisible ()
}

#' @title Set folders
#'
#' @description 
#' Sets folders for rdata, plots, tables, and analyses
#' 
#' @param ... multiple character scalars
#' @export
set_folders <- function (...) {
  set_rdata_folder (...)
  set_analyses_folder (...)
  set_plots_folder (...)
  set_tables_folder (...)
  
  invisible ()
}
