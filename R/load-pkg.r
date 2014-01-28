#' @title Loads package from hard drive
#'
#' @param pkg string of repository directory name
#' @param code_dir string of path to repository directory
#' @return Loads package
#' @export
load_pkg <- function (pkg = "poiscon", code_dir = options()$code_dir) {
  
  assert_that(is.string(pkg))
  assert_that(is.string(code_dir))
  
  dir <- paste(code_dir, pkg, pkg, sep = "/")
  
  if(!file.exists(dir))
    stop("directory", dir, "doesn't exist")
  
  load_all(dir, recompile = TRUE, export_all = FALSE)
}
