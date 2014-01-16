#' @title Extract figures
#'
#' @description
#' Extracts all the png files from output/plots/figures and saves to
#' output/report/project-name/figures ready for linking to the report and
#' posting on the jekyll site in the figures directory.
#' 
#' @param dir jekyll repository
#' @return An invisible logical scalar indicating whether or not sucessful.
#' @export
figures_web <- function (dir = "poissonconsulting.github.io") {
  
  assert_that(is.string(dir))
  assert_that(is.string(options()$code_dir))
  
  path <- options()$code_dir

  from <- "output/plots/figures"
  
  files <- list.files(from, pattern = "[.]png", full.names = TRUE, 
                      recursive = TRUE)
  
  if(length(files)) {
    
    to <- str_replace_all(files, 
                          "output/plots/figures",
                          paste(path, dir, "figures", project_folder(), 
                                sep = "/"))
    
    folders <- str_replace_all(to, "[/][^/]*[.][p][n][g]$", "")
    folders <- unique(folders)
    
    for (folder in folders) 
      dir.create(folder, recursive = TRUE, showWarnings = FALSE)
            
    file.copy(files, to, overwrite = TRUE)
  }
  return (invisible())
}
