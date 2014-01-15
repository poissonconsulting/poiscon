#' @title Publish poiscon
#'
#' @description
#' Currently just publishes slidify to clone of jekyll repo.
#' 
#' @param deck_dir string of deck directory (in output/decks).
#' @param repo_dir string of jekyll directory name.
#' @param repo_path string of path to jekyll directory
#' @return Transfers deck
publish_deck <- function (deck_dir = "deck",
                          repo_dir = "poissonconsulting.github.io", 
                          repo_path = "~/Documents/code") {

  assert_that(is.string(deck_dir))
  assert_that(is.string(repo_dir))
  assert_that(is.string(repo_path))
  
  from <- paste0("output/decks/", deck_dir)
  to <- paste0(repo_path, "/", repo_dir)
  
  if(!file.exists(from))
    stop(paste0("deck ", deck_dir, " does not exist"))
  
  if(!file.exists(to))
    stop("repository ", repo_dir, " does not exist")

  if(!file.exists(paste0(to, "/-posts")))
    stop("repository ", repo_dir, " is not a jekyll repository")
  
  message("Publishing deck ", from, "to ", to)
  
  return (invisible(file.copy(from, paste0(to, "/decks/", deck_dir), recursive = TRUE)))
}
