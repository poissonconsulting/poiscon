
get_rdata_folder <- function() {
  dir.create(paste0(getOption("folders.rdata_directory"), "/", getOption("folders.rdata_folder")), 
    recursive = T, showWarnings = F)
  
  if (is.null(getOption("folders.rdata_folder"))) 
    return(getOption("folders.rdata_directory"))
  return(paste0(getOption("folders.rdata_directory"), "/", getOption("folders.rdata_folder")))
}

get_analyses_folder <- function() {
  dir.create(paste0(getOption("folders.analyses_directory"), "/", getOption("folders.analyses_folder")), 
    recursive = T, showWarnings = F)
  
  if (is.null(getOption("folders.analyses_folder"))) 
    return(getOption("folders.analyses_directory"))
  return(paste0(getOption("folders.analyses_directory"), "/", getOption("folders.analyses_folder")))
}

get_plots_folder <- function(type = "figures") {
  stopifnot(type %in% c("data", "analyses", "figures", "slides"))
  
  dir.create(paste0(getOption("folders.plots_directory"), "/", type, "/", getOption("folders.plots_folder")), 
    recursive = T, showWarnings = F)
  
  if (is.null(getOption("folders.plots_folder"))) 
    return(paste0(getOption("folders.plots_directory"), "/", type))
  return(paste0(getOption("folders.plots_directory"), "/", type, "/", getOption("folders.plots_folder")))
}

get_tables_folder <- function(type = "results") {
  stopifnot(type %in% c("data", "analyses", "results"))
  
  dir.create(paste0(getOption("folders.tables_directory"), "/", type, "/", getOption("folders.tables_folder")), 
    recursive = T, showWarnings = F)
  
  if (is.null(getOption("folders.tables_folder"))) 
    return(paste0(getOption("folders.tables_directory"), "/", type))
  return(paste0(getOption("folders.tables_directory"), "/", type, "/", getOption("folders.tables_folder")))
} 

#' @title Get folders
#'
#' @description 
#' Sets folders for rdata, plots, tables, and analyses
#' 
#' @export
get_folders <- function () {
  getOption("folders.tables_folder")
}
