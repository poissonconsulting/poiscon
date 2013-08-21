
#' @export
write_bch_data <- function (data, file = NULL)
{
  
  if(is.null(file))
    file <- format(min(data$Timing), format = "%Y-%m-%d")
  
  file <- paste(file,"csv",sep=".")

  invisible(write.csv(data,file,row.names = FALSE))
}
