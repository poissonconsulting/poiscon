#' @title Tidy files
#'
#' @description
#' Tidies .r or .R files in working directory using \code{tidy.source} except
#' those which start with "model." The width.cutoff is set to be 80.
#' 
#' @export
tidy_files <- function () 
{
  flist = list.files(pattern = "^[^m][^o][^d][^e][^l].*[.][rR]$", full.names = TRUE
                     )
  for (f in flist) {
    message("tidying ", f)
    try(tidy.source(f, file = f, width.cutoff = 80))
  }
  invisible()
}
