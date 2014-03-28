#' @title Load table
#'
#' @description 
#' Reads in csv file from current table folder.
#' 
#' @param name the name of the table (csv extension is not required)
#' @param type the table type i.e. results (default) or analysis etc.
#' @return a data.frame of the table
#' @export
load_table <- function(name = "table", type = "results") {
  assert_that(is.string(name))
  assert_that(is.string(type))
  
  name <- replace_ext(name, "rds")
  file <- file.path(get_tables_folder(type = type), name)
  
  if (!file.exists(file)) 
    stop("the rds file associated with file ", replace_ext(file, "csv"), " does not exist")
  
  readRDS(file)$data
} 
