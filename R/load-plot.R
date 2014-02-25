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
  
  assert_that(is.string(name))
  assert_that(is.string(type))
  
  name <- replace_ext(name, "rds")
  
  file <- file.path(get_plots_folder(type = type), name)
  
  if (!file.exists (file))
    stop("the rds file associated with file ",replace_ext(file, "png")," does not exist")
    
  readRDS(file)$plot
}
