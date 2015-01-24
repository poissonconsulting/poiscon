
set_rdata_folder <- function(folder = NULL, dir = getOption("folders.rdata_directory")) {
  options(folders.rdata_folder = folder)
  options(folders.rdata_directory = dir)
}

set_analyses_folder <- function(folder = NULL, dir = getOption("folders.analyses_directory")) {
  options(folders.analyses_folder = folder)
  options(folders.analyses_directory = dir)
}

set_plots_folder <- function(folder = NULL, dir = getOption("folders.plots_directory")) {
  options(folders.plots_folder = folder)
  options(folders.plots_directory = dir)
}

set_tables_folder <- function(folder = NULL, dir = getOption("folders.tables_directory")) {
  options(folders.tables_folder = folder)
  options(folders.tables_directory = dir)
}

#' @title Set folders
#'
#' @description 
#' Sets folders for rdata, plots, tables, and analyses
#' 
#' @param ... multiple character scalars
#' @export
set_folders <- function(...) {
  
  old_folders <- get_folders()
  
  folder <- paste(..., sep = "/", collapse = "/")
  if (!length(folder)) 
    folder <- NULL
  
  set_rdata_folder(folder)
  set_analyses_folder(folder)
  set_plots_folder(folder)
  set_tables_folder(folder)
  invisible(folder)
} 
