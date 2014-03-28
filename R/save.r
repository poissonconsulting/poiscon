#' @title Save rdata
#'
#' @description 
#' Saves object as .rdata files in current rdata folder.
#' 
#' @param object the object to save.
#' @param name a character scalar of the name of the file.
#' @return Saves object as .rdata files in current rdata folder.
#' @export
save_rdata <- function(object, name = "data") {
  
  file <- paste0(get_rdata_folder(), "/", name, ".rds")
  
  saveRDS(object, file)
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
