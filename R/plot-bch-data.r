
#' @title Plotting BCH data
#'
#' @description
#' Plots environmental data
#'
#' @param data a data.frame of the BCH data to plot.
#' @param file a character scalar or NULL of the file name for the plot.
#' @return The data is written to a pdf file.
#' @export
plot_bch_data <- function (data, file = NULL)
{
  if (!inherits(data,"data.frame"))
    stop("data must be class data.frame")
  
  if(is.null(data$Variable))
    data$Variable <- data$Code

  if(is.null(data$Level))
    data$Level <- data$Mean
  
  if (!all(c("Timing","Variable","Level") %in% colnames(data)))
    stop("data must have columns Timing, Variable (or Code) and Level (or Mean)")
    
  variable <- unique(data$Variable)
  
  if(!"Status" %in% colnames(data))
    data$Status <- "Undefined"
  
  if(is.null(file)) {
    file <- format(min(data$Timing), format = "%Y-%m-%d")
  } else if(substr(file,nchar(file)-3,nchar(file)) == ".pdf")
    file <- substr(file,1,nchar(file)-4)
  
  pdf(paste0(file,".pdf"))
  on.exit(dev.off())
  
  for (i in 1:length(variable)) {
    dat <-  data[data$Variable == variable[i],]
    gp <- ggplot(data = dat, aes_string(x = "Timing", 
                                                          y = "Level", 
                                                          color = "Status"))
    gp <- gp + geom_line()
    gp <- gp + scale_x_datetime(name = "Date",labels = date_format("%Y-%m-%d"))  
    gp <- gp + scale_color_manual(values=c("black","grey50","red","blue","white"))  
    gp <- gp + ylab(paste(dat$Variable[1]))
    
    print(gp)
    
  }  
  return (invisible (data))
}
