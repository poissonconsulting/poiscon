#' Source windows
#' 
#' Requires that file be sourced in windows.
#' 
#' @param file a character scalar indicating the file to source
#' @param verbose a logical scalar indicating whether to print messages.
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

#' Knit Report
#' 
#' Knits report file. An inhouse function.
#' 
#' @param report a character scalar indicating the RMarkdown file
#' to knit.
#' @param quiet a logical scalar indicating whether to suppress messages.
#' @return An invisible logical scalar indicating if successful.
#' @export
knit_report <- function (report = "report.Rmd", quiet = FALSE) {
  stop("inhouse function")
}

#' Report to Jekyll Repository
#' 
#' Copies report to jekyll repository for publishing on website.
#' An inhouse function.
#' 
#' @param report a character scalar indicating the knitted 
#' RMarkdown file and associated images and figures to copy
#' to the jekyll repository.
#' @param jekyll_dir a character scalar of the path to the 
#' jekyll directory relative to the code directory.
#' @param code_dir a character scalar of the absolute path to the code directory.
#' @return An invisible logical scalar indicating if successful.
#' @export
report_to_jekyll <- function (report = "report.Rmd",
  jekyll_dir = getOption("reporting.jekyll.dir", "jekyll"),
  code_dir = getOption("reporting.code.dir", "~/Code")) {
  stop("inhouse function")
}
