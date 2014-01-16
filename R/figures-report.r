#' @title Extract figures
#'
#' @description
#' Extracts all the png files from output/plots/figures and saves to
#' output/report/project-name/figures ready for linking to the report and
#' posting on the jekyll site in the figures directory.
#' 
#' @return An invisible logical scalar indicating whether or not sucessful.
#' @export
figures_report <- function () {
  
  from <- "output/plots/figures"
  
  files <- list.files(from, pattern = "[.]png", full.names = TRUE, 
                      recursive = TRUE)
  
  if(length(files)) {
    
    to <- str_replace_all(files, "plots/figures", 
                          paste0("report/",project_folder(),"/figures"))
    
    folders <- str_replace_all(to, "[/][^/]*[.][p][n][g]$", "")
    folders <- unique(folders)
    
    for (folder in folders) 
      dir.create(folder, recursive = TRUE, showWarnings = FALSE)
    
    file.copy(files, to, overwrite = TRUE)
    file.copy(str_replace_all(files,"[.]png",".csv"), 
              str_replace_all(to,"[.]png",".csv"), overwrite = TRUE)
    
  }
  return (invisible())
}
