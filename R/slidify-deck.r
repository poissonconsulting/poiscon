#' @title Slidify deck
#'
#' @description
#' Slidifies deck
#' 
#' @param dir string of deck dir
#' @return Slidifies deck
#' @export
slidify_deck <- function (dir = "deck") {
  
  assert_that(is.string(dir))
  
  inputFile <- "index.Rmd"
  assert_that(file.exists(paste(dir, inputFile, sep = "/")))
  
  wd <- getwd()
  on.exit(setwd(wd))
  
  setwd(paste(wd, dir, sep = "/"))
  
  slidify(inputFile)
  
  browseURL(paste(wd, dir, str_replace(inputFile, "[.][Rr][Mm][Dd]", ".html"), sep = "/"))
  
  invisible(TRUE)
}