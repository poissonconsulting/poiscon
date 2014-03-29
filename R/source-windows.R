#' @export
source_windows <- function (file, verbose = getOption("verbose")) {
  assert_that(is.string(file))
  assert_that(is.flag(verbose))
  
  if(!is.windows())
    stop("script '", file, ",must be sourced in windows")
  
  if(!file.exists(file))
    stop("script '", file, "' not found")
  
  source(file, verbose = verbose)
}
