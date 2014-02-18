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
  
  
  assert_that(is.string(dir))
  
  path <- getOption("code_dir", "~/Documents/code")
  if(!is.string(path))
    stop("option()$code_dir must be a character scalar")
  
  message("Pushing jekyll web repository ", dir, " ...")
  
  git_commit(dir = paste0(path, "/", dir), message, push = TRUE)
  
  message("Repository published at http://", dir)
}
