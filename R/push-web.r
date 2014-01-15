#' @title Publishes web respository
#'
#' @description
#' Pushes web respository
#' 
#' @param dir string of jekyll repository directory
#' @param message string of commit message
#' @return Git commits
#' @export
push_web <- function (dir = "poissonconsulting.github.io",
                      message = paste0(project_folder(), " ",Sys.time())) {
  
  path <- options()$code_dir
  
  assert_that(is.string(dir))
  assert_that(is.string(path))
  
  message("Pushing jekyll web repository ", dir, " ...")
  
  git_commit(dir = paste0(path, "/", dir), message, push = TRUE)
  
  message("Repository published at http://", dir)
}
