
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
  
  if(is.null(file))
    file <- format(min(data$Timing), format = "%Y-%m-%d")
  
  pdf(paste0(file,".pdf"))
  on.exit(dev.off())
  
  for (i in 1:length(variable)) {
    dat <-  subset(data,Variable == variable[i])
    gp <- ggplot2::ggplot(data = dat, ggplot2::aes(x = Timing, y = Level, color = Status))
    gp <- gp + ggplot2::geom_line()
    gp <- gp + ggplot2::scale_x_datetime(name = "Date",labels = scales::date_format("%Y-%m-%d"))  
    gp <- gp + ggplot2::scale_color_manual(values=c("black","grey50","red","blue","white"))  
    gp <- gp + ggplot2::ylab(paste(dat$Variable[1]))
    
    print(gp)
    
  }  
  invisible (data)
}
