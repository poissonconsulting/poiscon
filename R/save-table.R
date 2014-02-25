#' @title Save table
#'
#' @description 
#' Saves data.frame as csv file in current table folder. Row names
#' are only save if informative (i.e. not NULL and not just 1:nrow(x)).
#' 
#' @param x the data.frame to to save
#' @param name character scalar of the name for the csv table
#' @param type character scalar of the type of folder to save in
#' @return Saves x as csv file in current table folder.
#' @export
save_table<-function (x, name = "table", type = "results") {
      
  assert_that(is.data.frame(x))
  assert_that(is.string(name))
  assert_that(is.string(type))
  
  if (type != "results")
    report <- FALSE 
  
  name <- replace_ext(name, "csv")
    
  file <- file.path(get_tables_folder(type=type), name)
  
  obj <- list(data = x)
  
  saveRDS(obj, file = replace_ext(file, "rds"))

  row.names <- row.names(x)
  row.names <- !(is.null(row.names) || identical(row.names,as.character(1:nrow(x))))
  
  write.csv(x, replace_ext(file, "csv"), row.names = row.names)
}
