
#' @title Load rdata
#'
#' @description 
#' Reads in .rdata file from current rdata folder.
#' 
#' @param name the name of the file (without extension).
#' @return an object
#' @export
load_rdata <- function(name = "data") {
  file <- paste0(get_rdata_folder(), "/", name, ".rds")
  
  if (file.exists(file)) 
    return(readRDS(file))
  warning(paste0("file ", file, " does not exist"))
  return(invisible())
}

#' @title Load analysis
#'
#' @description 
#' Reads in .rdata file from current analysis folder.
#' 
#' @param name the name of the file (without extension).
#' @return an analysis object
#' @export
load_analysis <- function(name = "analysis") {
  file <- paste0(get_analyses_folder(), "/", name, ".rds")
  if (file.exists(file)) 
    return(readRDS(file))
  warning(paste0("file ", file, " does not exist"))
  return(invisible())
} 
