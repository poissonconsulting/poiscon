#' @title Save rdata
#'
#' @description 
#' Saves object as .rdata files in current rdata folder.
#' 
#' @param object the object to save if unspecified the
#' function saves all data.frames in the global environment
#' @param name string of the name of the file if unspecified
#' the file is named the same as the object. If object is unspecified
#' name is ignored and each data.frame in the global environment is saved.
#' @return Invisible flag indicating if successful (also issues warning if FALSE).
#' @export

save_rdata <- function(object, name = NULL) {
  
  assert_that(is.null(name) || is.string(name))
  
  aname <- deparse(substitute(object))
  
  if(!identical(aname, "")) {
    if (is.null(name)) {
      name <- aname
    }
    file <- paste0(get_rdata_folder(), "/", name, ".rds")
    saveRDS(object, file)
    return (invisible(TRUE))
  }
  
  flag <- FALSE
  names <- objects(globalenv())
  for (name in names) {
    object <- globalenv()[[name]]
    if(is.data.frame(object)) {
      save_rdata(object, name = name)
      flag <- TRUE
    }
  }
  if(!flag)
    warning("no data frames in global environment")
  
  invisible(flag)
}

#' @title Save analysis
#'
#' @description 
#' Saves object as .rdata files in current analysis folder.
#' 
#' @param object the object to save.
#' @param name a character scalar of the name of the file.
#' @return Saves object as .rdata files in current analysis folder.
#' @export
save_analysis <- function(object, name = "analysis") {
  assert_that(is.jags_analysis(object))
  
  file <- paste0(get_analyses_folder(), "/", name, ".rds")
  
  saveRDS(object, file)
} 
