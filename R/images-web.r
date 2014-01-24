#' @title Images to web
#'
#' @description
#' Transfers all the files
#' output/report/project-name/images to the project folder in the
#' images directory on the jekyll site
#' 
#' @param dir jekyll repository
#' @return An invisible logical scalar indicating whether or not sucessful.
#' @export
images_web <- function (dir = "poissonconsulting.github.io") {
  
  assert_that(is.string(dir))
  assert_that(is.string(options()$code_dir))
  
  path <- options()$code_dir
  
  from <- "input/images"
  
  files <- list.files(from, pattern = "[.]png$|[.]jpg$", full.names = TRUE, 
                      recursive = TRUE)
  
  if(length(files)) {
    
    to <- str_replace_all(files, 
                          "input/images",
                          paste(path, dir, "images", project_folder(), 
                                sep = "/"))
    
    folders <- unique(dirname(to))
        
    for (folder in folders) 
      dir.create(folder, recursive = TRUE, showWarnings = FALSE)
    
    file.copy(files, to, overwrite = TRUE)
  }
  invisible(TRUE)
}
