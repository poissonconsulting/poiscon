#' Delete output folder
#'
#' Deletes output folder for current project.
#' @param check a logical scalar indicating whether to check with user before deleting
#' @return An invisible logical scalar indicating if successful.
#' @importFrom tulip delete_directory
#' @export
delete_output <- function (check = TRUE) {
  delete_directory(file.path(getwd(), "output"), check = check)
}

#' Delete plots folder
#'
#' Deletes plots folder for current project.
#' @param check a logical scalar indicating whether to check with user before deleting
#' @return An invisible logical scalar indicating if successful.
#' @importFrom tulip delete_directory
#' @export
delete_plots <- function (check = TRUE) {
  delete_directory(file.path(getwd(), "output", "plots"), check = check)
}
