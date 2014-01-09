#' @title Project folder
#'
#' @description
#' Returns the name of the parent folder. It's uses include
#' determining the name of the current project (the parent directory
#' should have the same name as the associated github repository).
#' 
#' @return The name of the project folder as a character scalar.
#' @export
project_folder <- function () {
  x <- strsplit(getwd(), "/")[[1]]
  return (x[length(x)])
}
