#' @title Installs package from hard drive
#'
#' @description
#' Commits changes to github repository and pushes to remote
#' 
#' @param pkg string of repository directory name
#' @param code_dir string of path to repository directory
#' @return Installs package
#' @export
install_hd <- function (pkg = "poiscon", code_dir = options()$code_dir) {
  
  assert_that(is.string(pkg))
  assert_that(is.string(code_dir))
  
  dir <- paste(code_dir, pkg, pkg, sep = "/")
  
  if(!file.exists(dir))
    stop("directory", dir, "doesn't exist")
  
  return (install(dir))
}
