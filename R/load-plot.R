#' @title Load plot
#'
#' @description 
#' Reads in ggplot object
#' 
#' @param name character scalar of the name of the plot (without extension).
#' @param type character scalar indicating the plot type i.e. figures (default) or analysis, data etc.
#' @param dataset logical scalar indicating whether to return the data as opposed to the ggplot object
#' @return the gggplot object or the plot data
#' @export
load_plot <- function(name = "plot", type = "figures", dataset = FALSE) {
  
  assert_that(is.string(name))
  assert_that(is.string(type))
  assert_that(is.flag(dataset) && noNA(dataset))
  
  name <- replace_ext(name, "rds")
  
  file <- file.path(get_plots_folder(type = type), name)
  
  if (!file.exists(file)) 
    stop("the rds file associated with file ", replace_ext(file, "png"), " does not exist")
  
  if (dataset) 
    return(readRDS(file)$plot$data)
  
  readRDS(file)$plot
} 
