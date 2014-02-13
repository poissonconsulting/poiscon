
#' @title Load rdata
#'
#' @description 
#' Reads in .rdata file from current rdata folder.
#' 
#' @param name the name of the file (without extension).
#' @return an object
#' @export
load_rdata <- function (name = 'data') {
  file <- paste0(get_rdata_folder(), '/', name, '.rds') 

  if (file.exists (file))
    return (readRDS(file))
  warning (paste0("file ",file," does not exist"))
  return (invisible())
}

#' @title Load analysis
#'
#' @description 
#' Reads in .rdata file from current analysis folder.
#' 
#' @param name the name of the file (without extension).
#' @return an analysis object
#' @export
load_analysis <- function (name = 'analysis') {
  file <- paste0(get_analyses_folder(), '/', name, '.rds')
  if (file.exists (file))
    return (readRDS(file))
  warning (paste0("file ",file," does not exist"))
  return (invisible())
}

#' @title Load plot
#'
#' @description 
#' Reads in ggplot object
#' 
#' @param name character scalar of the name of the plot (without extension).
#' @param type the plot type i.e. figures (default) or analysis, data etc.
#' @return a data.frame object
#' @export
load_plot <- function (name = "plot", type = "figures") {
  
  file <- paste0(get_plots_folder(type = type), "/", name, ".rds")

  if (!file.exists (file)) {
    warning ("plot ", file, " does not exist")
    return (NULL)
  }
  gplot <-readRDS(file)
  if(!is.gplot(gplot)) {
    warning ("plot ", file, " is of class ", class(gplot))
  }
    
  gplot$ggplot
}

#' @title Load table
#'
#' @description 
#' Reads in table.csv file from current table folder.
#' 
#' @param name the name of the table (without extension).
#' @param type the table type i.e. results (default) or analysis etc.
#' @return a data.frame object
#' @export
load_table <- function (name = "table", type = "results") {
  
  file <- paste0(get_tables_folder(type = type), "/", name, ".csv")
  if (file.exists (file))
    return (read.csv(file))
  warning (paste0("file ",file," does not exist"))
  return (invisible())
}
