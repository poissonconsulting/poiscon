#' @title Deck to web repository
#'
#' @description
#' Copies slide deck to jekyll site
#' 
#' @param dir a string of the slide deck directory
#' @param release_date a Date scalar of the date the deck was finalized or NULL
#' @param web_dir a string of the web repository
#' @return deck to web
#' @export
deck_to_web <- function (release_date = NULL, dir = "deck",
                         web_dir = "poissonconsulting.github.io") {
  
  assert_that(is.string(dir))
  assert_that(is.null(release_date) || is.Date(release_date))
  assert_that(is.string(web_dir))
  
  path <- getOption("code_dir", "~/Documents/code")

  assert_that(is.string(path))
  
  assert_that(file.exists(dir))
  assert_that(file.exists(paste(path, web_dir, sep = "/")))
  
  project_folder <- project_folder()
  
  wd <- getwd()
  on.exit(setwd(wd))
  
  setwd(paste(wd, dir, sep = "/"))
  
  
  if (!file.exists("libraries")) {
    message("Please set mode to selfcontained and run slidify_deck")
    message("This would place library files in the slide folder")
    message("making it self-contained")
    return (invisible(FALSE))
  }
  if (!file.exists(".nojekyll")) {
    message("Adding .nojekyll to the deck...")
    file.create(".nojekyll")
  }

  if (is.null(release_date)) {
    to <- paste(path, web_dir, "temporary-hidden-link", 
                murmur3.32(project_folder), project_folder, dir, sep = "/")
  } else {
      to <- paste(path, web_dir, "presentations", 
                  format(release_date, format = "%Y/%m/%d"),
                project_folder, sep = "/")
      if (dir != "deck") {
        to <- paste(to, dir, sep = "/")
      }
  }
  dir.create(to, showWarnings = FALSE, recursive = TRUE)
  
  message("Copying files to ", to)
  
  str_replace_file("index.html", 
                   "=\"libraries/", 
                   "=\"/libraries/",
                   to = "temporary.html")
  
  file.copy("temporary.html", paste0(to, "/index.html"), overwrite = TRUE)
  file.remove("temporary.html")

  file.copy("assets", to, overwrite = TRUE, recursive = TRUE)
  
  file.copy(".nojekyll", paste0(to, "/.nojekyll"), overwrite = TRUE)
  
  if(!file.exists(paste(path, web_dir, "libraries", sep = "/")))
    dir.create(paste(path, web_dir, "libraries", sep = "/"))
  
  file.copy("libraries", paste(path, web_dir, sep = "/"),
            overwrite = TRUE, recursive = TRUE)
  
  url <- str_replace(paste0(to, "/"), path, "http:/")
  
  cat(url)
  
  invisible(TRUE)
}
