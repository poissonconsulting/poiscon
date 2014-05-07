#' Delete output folder
#'
#' Deletes output folder for current project.
#' @param delete_rdata_input flag indicating whether to delete the output/rdata/input
#' folder which in the PCL system is used to store the initial input of data
#' @param check flag indicating whether to confirm with user before deleting
#' @return An invisible logical scalar indicating if successful.
#' @export
delete_output <- function (delete_rdata_input = TRUE, check = TRUE) {
  assert_that(is.flag(delete_rdata_input) && noNA(delete_rdata_input))
  
  dir <- file.path(getwd(), "output")
  flag <- TRUE
  if(!delete_rdata_input) {
    flag <- copy_directory(file.path(dir,"rdata","input"), parent_dir = tempdir(),
      check = FALSE)
  }
  
  if(flag) {
    flag <- delete_directory(dir, check = check)
    if (flag && !delete_rdata_input) {
      flag <- copy_directory(dir = file.path(tempdir(), "rdata", "input"), parent_dir = file.path(getwd(), "output", "rdata"), check = FALSE)
    }
  }
  if(!flag)
    warning("delete output failed")
  invisible(flag)
}

#' Delete plots folder
#'
#' Deletes plots folder for current project.
#' @param check a logical scalar indicating whether to check with user before deleting
#' @return An invisible logical scalar indicating if successful.
#' @export
delete_plots <- function (check = TRUE) {
  delete_directory(file.path(getwd(), "output", "plots"), check = check)
}
