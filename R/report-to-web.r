#' @title Report to web repository
#'
#' @description
#' Copies project report files to jekyll site
#' 
#' @param dir jekyll repository
#' @return Uploads files to jekyll site on my harddrive.
#' @export
report_to_web <- function (dir = "poissonconsulting-jekyll") {

  assert_that(is.string(dir))
  
  path <- getOption("code_dir", "~/Documents/code")

  assert_that(path)

  from <- paste0("output/report/", project_folder(), "/report.md")
  
  if(!file.exists(from))
    stop("file", from, "does not exist")
  
  layout <- header_setting(from, "layout")
  
  if(is.null(layout))
    stop("file", from, "does not have a jekyll header")
  
  stopifnot(layout %in% c("page", "post"))
        
  if(layout == "page") {
    
    to <- paste0(path, "/", dir, "/", "temporary-hidden-link/",
                 murmur3.32(project_folder()), "/", project_folder())
    
    dir.create(to, showWarnings = FALSE,
               recursive = TRUE)
    
    url <- to
    
  } else if (layout == "post") {
    
    date <- as.Date(header_setting(from, "release_date"))
    
    stopifnot(is.Date(date))
    
    to <- paste0(path, "/", dir, "/", "_posts/",
                 format(date, format = "%Y-%m-%d-"), project_folder())
    
    url <- paste0(path, "/", dir, "/analyses/",
                  format(date, format = "%Y/%m/%d/"), project_folder())    
  }
  
  url <- str_replace(url, path, "http:/")
  cat(url)
  
  "src = \"figures/"

  str_replace_file(from, 
                   "src = \"figures/", 
                   paste0("src = \"/figures/", project_folder(), "/"))

  str_replace_file(from, 
                   "\\]\\(input/images/", 
                   paste0("\\]\\(/images/", project_folder(), "/"))
  
  if(layout == "page") {
    file.copy(from, paste0(to, "/index.md"), overwrite = TRUE)
  } else {
    file.copy(from, paste0(to, ".md"), overwrite = TRUE)
  }  
  figures_web(dir)
  images_web(dir)
  return (invisible(TRUE))
}
