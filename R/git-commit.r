#' @title Git commit (and push)
#'
#' @description
#' Commits (and pushes) changes
#' 
#' @param dir string of repository directory
#' @param message string of commit message
#' @param pash flag of whether to push to remote
#' @return Git commits
#' @export
git_commit <- function (dir = getwd(),
                         message = paste0("poiscon::git_commit: ", Sys.time()),
                         push = TRUE) {
  
  assert_that(is.string(dir))
  assert_that(is.string(message))
  assert_that(is.flag(push))
    
  if(!file.exists(dir))
    stop("repository ", dir, " doesn't exist")
  
  if (!file.exists(paste0(dir, "/.git")))
    stop("repository is not a git repository")
  
  wd <- getwd()
  on.exit(setwd(wd))
  
  setwd(dir)
  system("git add .")
  system(paste0("git commit -a -m \"", message, "\""))
  
  if(push)
    system("git push -u origin HEAD")
  
  return(invisible())
}
