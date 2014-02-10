#' @title List assumptions
#'
#' @description
#' Takes assumptions character vector in file models-'model'.r
#' and cats as unordered list in markdown
#' 
#' @param model character scalar of analysis part of model name
#' @return logical scalar indicating whether successful
#' @export
list_assumptions <- function (model) {
  assert_that(is.string(model))
  
  file <- paste0("models-", model, ".r")
  if(!file.exists(file))
    stop("file", file, "doesn't exist")
  source(file)
  if(!exists("assumptions"))
    stop("assumptions are not defined in", file)
  
  if(!is.character(assumptions))
    stop("assumptions must be a character vector")
  
  assumptions <- paste("-", assumptions)
  for (a in assumptions) {
    cat("\n", a)
  }
  cat("\n")
  invisible(TRUE)
}
