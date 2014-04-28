#' @title Load rdata
#'
#' @description 
#' Reads in R data file from current rdata folder.
#' 
#' @param name the name of the file (without extension) to load. If
#' unspecified then loads all R data files in current rdata folder
#' into the global environment.
#' @return the object if name is specified otherwise loads
#' all objects in current rdata folder into global workspace
#' @export
load_rdata <- function(name = NULL) {
  
  assert_that(is.null(name) || is.string(name))
  
  if(!is.null(name)) {
    file <- paste0(get_rdata_folder(), "/", name, ".rds")
    
    if (file.exists(file)) { 
      object <- readRDS(file)
      return (object)
    }
    warning(paste0("object '", file, "' not found"))
    return (invisible())
  }
  files <- list.files(path = get_rdata_folder(), pattern = ".rds$")
  if(identical(files, character(0))) {
    warning("no objects found")
    return (invisible())
  }
  for (file in files) {
    name <- sub(".rds", "", basename(file))
    assign(name, load_rdata(name), globalenv())
  }
  invisible()
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
