#' @title Slidify deck
#'
#' @description
#' Slidifies deck
#' 
#' @param dir string of deck dir
#' @return Slidifies deck
slidify_deck <- function (dir = "deck") {
  
  wd <- getwd()
  on.exit(setwd(wd))
  
  setwd(paste0("output/decks/", dir))
  return (slidify("index.Rmd"))
}
