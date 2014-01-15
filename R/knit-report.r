#' @title Knits report
#'
#' @description
#' Knits report
#' 
#' @param web_dir a character scalar or a Date
#' @return Knits readme file into markdown
#' @export
knit_report <- function (file = "report.rmd") {
    
  assert_that(is.string(file))
  assert_that(file.exists(file))
  
  to <- paste0("output/report/", project_folder(), ".md")
  
  .figures <- FALSE
  knit(file, output = to)
  str_replace_file(to, "&", "and")
  # need to replace .md with \.docx for pandoc
  system(paste0("pandoc -s ", to," -o ",output_dir,".docx"))

  .figures <- TRUE
  knit(file, output = to)
  str_replace_file(to, "&", "and")
  # need to shunt up - delete blank lines...
#  text <- str_replace_all(text[-(1:2)], "&", "and")

  return (invisible())
}
