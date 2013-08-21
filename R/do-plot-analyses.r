
do_plot_analyses<-function (name, species, plot_analysis) {
  
  if(!is.function (plot_analysis))
    stop("plot_analyses should be a function")
  type <- getOption ('Poisson.analysis_type','full')
  
  if (type %in% c('first','debug')) {
    species <- sort(species)[1]
  } else if (type == 'remainder') {
    species <- species[species != sort(species)[1]]
  }
  
  for (spp in species)
  {
    print(spp)
    graphics.off()
    
    set_folders(name, spp)
    
    analysis<-load_analysis()
    
    print(summary(analysis))
    save_tables(analysis)
    save_plots(analysis)
    
    plot_analysis (analysis)
  }
}
