
#' @export
export_plots <- function () {
  
  if (.Platform$OS.type == "unix") {
    windows <- function (width = 7, height = 7,...) {
      quartz (width=width, height=height, ...)
    }
  }
  
  files <- list.files('../plots/figures',recursive=T)
  files <- files[substr(files,nchar(files)-3,nchar(files)) %in% c('.rds','.csv')]
  
  for (file in files) {
    dir <- strsplit(file,'/')[[1]]
    dir <- dir[-length(dir)]
    dir <- paste(dir,collapse='/')
    dir.create(paste0('../plots/export/',dir), showWarnings = F, recursive = T)
    if (substr(file, nchar(file)-3,nchar(file)) == '.csv') {
      file.copy(paste0('../plots/figures/',file),paste0('../plots/export/',file))
    } else if (substr(file, nchar(file)-3,nchar(file)) == '.rds') {
      gp <- readRDS(file=(paste0('../plots/figures/',file)))
      width <- gp$width
      height <- gp$height
      dpi <- gp$dpi
      gp <- gp$ggplot
      
      windows (width = width, height = height)
      err <- try(print(gp))
      
      if(class(err) != "try-error") {
      ggsave(paste0('../plots/export/',substr(file,1,nchar(file)-4), '.png'), width = width, height = height, dpi = dpi)
      
      graphics.off()  
      } else {
        print (err)
      }
      
      
    }
  }
}
