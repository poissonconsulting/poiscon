#' @title Knits README file
#'
#' @description
#' Knits readme
#' 
#' @param file string of file
#' @return Knits readme file into markdown
#' @export
knit_readme <- function (file = "README.rmd") {
    
  assert_that(is.string(file))
  assert_that(file.exists(file))
  
  return (knit(file))
}
