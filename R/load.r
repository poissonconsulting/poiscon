
#' @export
load_rdata <- function (name = 'data') {
  file <- paste0(get_rdata_folder(), '/', name, '.rds') 

  if (file.exists (file))
    return (readRDS(file))
  warning (paste0("file ",file," does not exist"))
  return (invisible (NULL))
}

#' @export
load_analysis <- function (name = 'analysis') {
  file <- paste0(get_analyses_folder(), '/', name, '.rds')
  if (file.exists (file))
    return (readRDS(file))
  warning (paste0("file ",file," does not exist"))
  return (invisible (NULL))
}

