#' @title Write BCH data
#'
#' @description 
#' Writes BCH data to csv files - creating file name from the date if unspecified
#' 
#' @param data the BCH data to write
#' @param file NULL or a character scalar of the files name without ".csv"
#' @return The results of write.csv
#' @export
write_bch_data <- function (data, file = NULL) {
  
  assert_that(is.data.frame(data))  
  assert_that(is.null(file) || is.string(file))
  
  if(is.null(file))
    file <- format(min(data$Timing), format = "%Y-%m-%d")
  
  file <- paste(file,"csv",sep=".")
  
  return (invisible(write.csv(data,file,row.names = FALSE)))
}
