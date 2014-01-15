#' @title Commit (and push) repository
#'
#' @description
#' Commits changes to github repository and pushes to remote
#' 
#' @param repo_dir string of repository directory name
#' @param repo_path string of path to repository directory
#' @param message string of commit message
#' @param push a logical scalar indicating whether or not to push
#' @return Transfers deck
#' @export
git_commit <- function (dir = getwd(),
                         message = paste0("poiscon::git_commit: ", Sys.time()),
                         push = TRUE) {
  
  assert_that(is.string(dir))
  assert_that(is.string(message))
  assert_that(is.flag(push))
    
  if(!file.exists(dir))
    stop("repository doesn't exist")
  
  if (!file.exists(paste0(dir, "/.git")))
    stop("repository is not a git repository")
  
  wd <- getwd()
  on.exit(setwd(wd))
  
  setwd(dir)
  system(paste0("git commit -a -m \"", message, "\""))
  
  if(push)
    system("git push -u origin HEAD")
  
  return(invisible())
}
