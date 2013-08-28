
#' @export
load_rdata <- function (name = 'data') {
  file <- paste0(get_rdata_folder(), '/', name, '.rds') 

  if (file.exists (file))
    return (readRDS(file))
  warning (paste0("file ",file," does not exist"))
  return (invisible (NULL))
}

#' @export
load_analysis <- function (name = 'analysis') {
  file <- paste0(get_analyses_folder(), '/', name, '.rds')
  if (file.exists (file))
    return (readRDS(file))
  warning (paste0("file ",file," does not exist"))
  return (invisible (NULL))
}

#' @title Load plot
#'
#' @description 
#' In current form reads in plot .csv file from current plot folder.
#' 
#' @param name the name of the plot (without extension).
#' @param type the plot type i.e. figures (default) or analysis, slides etc.
#' @param ext the extension of the file type to read in i.e. ".rds" the 
#' default or ".csv" the only option currently implemented.
#' @return a data.frame object
#' @references 
#' @export
load_plot <- function (name='plot', type='figures', ext = "rds") {

  if(!identical(ext,"csv"))
    stop("ext argument currently only accepts \"csv\"")
  
  file <- paste0(get_plots_folder(type = type), '/', name, '.csv')
  if (file.exists (file))
    return (read.csv(file))
  warning (paste0("file ",file," does not exist"))
  return (invisible (NULL))
}

