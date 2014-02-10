#' @title Increment version number
#'
#' @description
#' Increments description files version number and date
#' 
#' @return logical scalar indicating whether successful
#' @references Modified from http://rmflight.github.io/posts/2014/02/incrementalVersionCommitHooks.html
#' @export
increment_version_number <- function () {
  if(!file.exists("DESCRIPTION")) {
    warning("DESCRIPTION file doesn't exist")
    return (FALSE)
  }
  
  currDCF <- read.dcf("DESCRIPTION")
  currVersion <- currDCF[1, "Version"]
  splitVersion <- strsplit(currVersion, ".", fixed = TRUE)[[1]]
  nVer <- length(splitVersion)
  currEndVersion <- as.integer(splitVersion[nVer])
  newEndVersion <- as.character(currEndVersion + 1)
  splitVersion[nVer] <- newEndVersion
  newVersion <- paste(splitVersion, collapse = ".")
  currDCF[1, "Version"] <- newVersion
  currDCF[1, "Date"] <- as.character(Sys.Date())
  write.dcf(currDCF, "DESCRIPTION")
  
  TRUE
}
