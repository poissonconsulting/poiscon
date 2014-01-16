author_poiscon <- function (deck_dir = "output/deck",
                             jekyll_dir = "poissonconsulting.github.io", 
                             jekyll_path = "~/Documents/code") {
  
  if(!file.exists(jekyll_path))
    stop("jekyll_path does not exist")
  
  if(!file.exists(paste0(jekyll_path,"/",deck_dir)))
    stop("jekyll_dir does not exist")
  
  if(!file.exists(paste0(jekyll_path,"/",jekyll_dir,"/-posts")))
    stop("jekyll_dir is not a jekyll repository")
  
  return (paste0(jekyll_path,"/",jekyll_dir))
  
}
