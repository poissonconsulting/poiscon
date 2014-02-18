#' @title Install package from hard drive
#' 
#' @param pkg string of repository directory name
#' @param code_dir string of path to repository directory
#' @return Installs package
#' @export
install_hd <- function (pkg = "poiscon", code_dir = getOption("code_dir", "~/Documents/code")) {
  
  warning("install_hd has been deprecated for load_pkg in v0.7")
  load_pkg(pkg, code_dir)
}
