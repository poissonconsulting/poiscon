
do_analysis <- function (name, species, models, n.iter = 10^3, debug = F) 
{
    
  for (spp in species) {        
    cat(paste0("\nSpecies: ",spp,"\n"))

    set_folders(name, spp)    
    
    data <- load_rdata()
            
    analysis <- janalysis(
      models, data, n.iter=n.iter, debug= debug
    ) 

    save_analysis(analysis)
    
    print(summary(analysis))
    save_tables(analysis)
    save_plots(analysis)  
  }    
}

do_plot_analysis<-function (name, species, plot_analysis) {
  
  if(!is.function (plot_analysis))
    stop("plot_analysis should be a function")

  
  for (spp in species)
  {
    cat(paste0("\nSpecies: ",spp,"\n"))

    graphics.off()
    
    set_folders(name, spp)
    
    analysis <- load_analysis()
    
    print(summary(analysis))
    plot_analysis (analysis)

    save_tables(analysis)
    save_plots(analysis)
  }
}

get_spp <- function () {
  folder <- getOption ("folders.analyses_folder")
  spp <- strsplit(folder, "/")[[1]][2]
  return (spp)
}
