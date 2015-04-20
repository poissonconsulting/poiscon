#' Remove Dots Colnames
#' 
#' Goes through all the data.frame objects in
#' the current environment and removes any dots
#' from the colnames
#' @export
remove_dots_colnames_data_frames <- function () {
  for(obj in ls(envir = parent.frame())) {
    expr <- parse(text = paste0(
      "if(is.data.frame(", obj, ")) {", 
      "\ncolnames(", obj, ") <- make.names(colnames(", obj, "))",
      "\ncolnames(", obj, ") <- gsub(\"[.]\", \"\", colnames(", obj, "))",
      "\n}"))
    eval(expr, envir = parent.frame())
  }
  invisible(TRUE)
}
