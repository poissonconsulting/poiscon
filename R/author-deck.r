#' @title Author deck
#'
#' @description
#' Creates slidify slide deck
#' 
#' @param dir direcotyr in which to create deck
#' @return flag indicating whether successful
#' @export
author_deck <- function (dir = "deck") {
  
  assert_that(is.string(dir))
  
  scaffold <- system.file("skeleton", package = "slidify")
  
  if(file.exists(dir)) {
    warning("Deck \"", dir, "\" not created as already exists")
    return (invisible(FALSE))
  }
  message("Creating slide directory at ", dir)
  dir.create(dir, recursive = TRUE)  
  invisible(all(file.copy(list.files(scaffold, full.names = T), dir, recursive = TRUE)))
}
