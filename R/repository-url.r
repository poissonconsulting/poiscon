#' @title Poisson repository URL
#'
#' @description
#' Returns the URL of the poisson repository corresponding to the
#' current project.
#'
#' @return The url of the poisson repository.
#' @export
repository_url <- function () {
  
  url <- paste0("https://github.com/poissonconsulting/",
                project_folder())
  
  return (url)
}
