#' @title Initialize deck
#'
#' @description
#' Initializes deck using slidify - based on author
#' 
#' @param dir string of deck dir
#' @return Initializes deck
initialize_deck <- function (dir = "deck") {
  
  assert_that(is.string(dir))

  to <- paste0("output/decks/", dir)
  
  if(file.exists(to))
    stop("deck already exists")
  
  from <- system.file("skeleton", package = "slidify")
  
  
  message("Creating slide directory at ", to, " ...")
  copy_dir(from, to)
  message("Finished creating slide directory...")  
  return (invisible())
}
