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
  
  to <- paste0("output/report/", project_folder(), "/report.md")
  
  if(!file.exists(str_replace(to, "/report.md", "")))
    dir.create(str_replace(to, "/report.md", ""), FALSE, recursive = TRUE)
  
  knit(file, output = to)
  
  from <- to
  to <- str_replace(to, "report.md", "temporary.md")
    
  rm_blank_lines_top(from)
  str_replace_file(from, "&", "and")
  
  str_replace_file(from, 
                   "[#][#].[B][a][c][k][g][r][o][u][n][d]", 
                   paste0("#### ", Sys.Date(), "\n## Background"), to = to)
  
  markdownToHTML(to, str_replace(from, "*[.][m][d]$", ".html"))
    
  system(paste0("pandoc -s ", to," -o ", str_replace(from,"[.][m][d]$",".docx")))

  file.remove(to)

  return (invisible())
}
