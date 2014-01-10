#' @title Upload files
#'
#' @description
#' Uploads files to jekyll site on my harddrive.
#' 
#' @param web_dir a character scalar or a Date
#' @return Uploads files to jekyll site on my harddrive.
#' @export
upload_files <- function (web_dir) {
  assert_that(is.Date(web_dir) || is.string(web_dir))
  
  from <- paste0("output/report/", project_folder(), ".md")
  
  if(is.string(web_dir)) {
    
    to <- str_replace(from, 
                      "output/report", 
                      paste0("~/Documents/code/poissonconsulting.github.io/",
                             "temporary-hidden-link/", web_dir))
    dir.create(str_replace(to,"[/][^/]*.[.][m][d]$",""), showWarnings = FALSE,
               recursive = TRUE)
    
    url <- str_replace(to,".md",".html")
    url <- str_replace(url,"~/Documents/code/","http://")
    cat(url)
    
  } else {
    to <- str_replace(from, 
                      "output/report/", 
                      paste0("~/Documents/code/poissonconsulting.github.io/",
                             "_drafts/",format(web_dir,format = "%Y-%m-%d-")))    
  }
  
  bol1 <- file.copy(from, to, overwrite = TRUE)
  
  from <- paste0("output/report/figures")
  to <- paste0("~/Documents/code/poissonconsulting.github.io/figures/",
               project_folder())
  
  dir.create(to, showWarnings = FALSE, recursive = TRUE)
  
  to <- str_replace(to, "[/][^/]*[/][^/]*$", "")
  
  bol2 <- file.copy(from, to, recursive = TRUE)
  return (invisible(c(bol1, bol2)))
}
