#' @title Extract figures
#'
#' @description
#' Extracts all the png files from output/plots/figures and saves to
#' output/report/figures/project-name ready for linking to the report and
#' posting on the jekyll site in the figures directory.
#' 
#' @return An invisible logical scalar indicating whether or not sucessful.
#' @export
extract_figures <- function () {
  
  folder <- poiscon:::get_plots_folder(type = "figures")
  files <- list.files(folder, pattern = "[.]png", full.names = TRUE, recursive = TRUE)
  
  if(length(files) > 0) {
    
    # hack to remove "//"
    files <- str_replace_all(files, "//", "/")
    
    to <- str_replace_all(files, "output/plots/figures", 
                          paste0("output/report/figures/",project_folder()))
    
    folders <- str_replace_all(to, "[/][^/]*[.][p][n][g]$", "")
    folders <- unique(folders)
    
    for (folder in folders) 
      dir.create(folder, recursive = TRUE, showWarnings = FALSE)
    
    return(invisible(file.copy(files, to, overwrite = TRUE)))
  }
  message("no figures to extract")
  return (invisible(TRUE))
}
