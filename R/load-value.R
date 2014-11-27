#' @title Load table
#'
#' @description 
#' Reads in csv file from current table folder.
#' 
#' @param rowname string of name of row containing value
#' @param colname string of name of column containing value
#' @param folders string of folders containing table - separate folders with '/'
#' @param name the name of the table (csv extension is not required)
#' @param type the table type i.e. results (default) or analysis etc.
#' @param digits count of number of significant digits
#' @return a data.frame of the table
#' @export
load_value <- function(rowname, colname = "estimate", folders = get_folders(), name = "estimates", type = "results", digits = 6) {
  
  assert_that(is.string(colname))
  assert_that(is.string(rowname))
  assert_that(is.string(name))
  assert_that(is.string(type))
  assert_that(is.count(digits))
  
  on.exit(set_folders(get_folders()))
  set_folders(folders)
  
  table <- load_table(name = name, type = type)
  
  assert_that(!anyDuplicated(colnames(table)))
  assert_that(!anyDuplicated(rownames(table)))
  
  assert_that(colname %in% colnames(table))
  assert_that(rowname %in% rownames(table))
  
  signif(table[rowname,colname,drop = TRUE], digits = digits)
} 
