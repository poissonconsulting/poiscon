
#' @export
read_bch_data <- function (file, recursive = TRUE, quiet = TRUE)
{
  return(read_zrxp(file,recursive = recursive, quiet = quiet))
}
