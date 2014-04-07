#' Delete output folder
#'
#' Deletes output directory in current working directory.
#' @param check a logical scalar indicating whether to check with user before deleting
#' @return An invisible logical scalar indicating if successful.
#' @export
delete_output_folder <- function (check = TRUE) {
  dir <- file.path(getwd(), "output")
  ## check
  message("deleting dir '", dir, "' ...")
  unlink(dir, recursive = TRUE)
  invisible(TRUE)
}

#' Delete plots folder
#'
#' Deletes output/plot directory in current working directory.
#' @param check a logical scalar indicating whether to check with user before deleting
#' @return An invisible logical scalar indicating if successful.
#' @export
delete_plots_folder <- function (check = TRUE) {
  dir <- file.path(getwd(), "output", "plots")
  ## check
  message("deleting dir '", dir, "' ...")
  unlink(dir, recursive = TRUE)
  invisible(TRUE)
}
