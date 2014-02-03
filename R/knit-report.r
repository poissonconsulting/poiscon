#' @title Knits report
#'
#' @description
#' Knits report
#' 
#' @param file a character scalar of file
#' @return Knits readme file into markdown
#' @export
knit_report <- function (file = "report.rmd") {
    
  assert_that(is.string(file))
  assert_that(file.exists(file))
  
  figures_report()
  
  to <- paste0("output/report/", project_folder(), "/report.md")
  
  if(!file.exists(str_replace(to, "/report.md", "")))
    dir.create(str_replace(to, "/report.md", ""), FALSE, recursive = TRUE)
  
  if(file.exists("input/images")) {
    file.copy("input/images", str_replace(to, "/report.md", ""), 
              overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
  }
  
  render_jekyll()
  knit(file, output = to)
    
  from <- to
  to_html <- str_replace(to, "report.md", "html.md")
  to_docx <- str_replace(to, "report.md", "docx.md")
  rm(to)
  
  rm_blank_lines_top(from)
  str_replace_file(from, "&", "and")
  
  title <- header_setting(from, "title")
  layout <- header_setting(from, "layout")
  
  stopifnot(layout %in% c("page", "post"))

  
  if(layout == "page") {
    str_replace_file(from, 
                     "[#][#][ ][B][a][c][k][g][r][o][u][n][d]", 
                     paste0("# ", title, "\n\n### Draft: ", Sys.time(), 
                            "\n\n## Background"),
                     to = to_html)  
    
    str_replace_file(from, 
                     "[#][#][ ][B][a][c][k][g][r][o][u][n][d]", 
                     paste0("### Draft: ", Sys.time(), "\n\n## Background"),
                     to = to_docx)    
    
    str_replace_file(from, 
                     "[#][#][ ][B][a][c][k][g][r][o][u][n][d]", 
                     paste0("### Draft: ", Sys.time(), "\n\n## Background"))      
  } else {
    date <- as.Date(header_setting(from, "release_date"))
    
    stopifnot(is.Date(date))
    
    str_replace_file(from, 
                     "[{][%][ ][i][n][c][l][u][d][e][ ][J][B][/][s][e][t][u][p][ ][%]}",
                     "",
                     to = to_docx)

    str_replace_file(from, 
                     "[{][%][ ][i][n][c][l][u][d][e][ ][J][B][/][s][e][t][u][p][ ][%]}",
                     paste("\n\n#", title, "\n\n###", date, "\n\n"),
                     to = to_html)  

    str_replace_file(from, 
                     "[{][%][ ][i][n][c][l][u][d][e][ ][J][B][/][s][e][t][u][p][ ][%]}",
                     paste("\n\n###", date, "\n\n"),
                     to = to_docx)
  }
  
  markdownToHTML(to_html, str_replace(from, "*[.][m][d]$", ".html"))
    
  system(paste0("pandoc -s ", to_docx," -o ", str_replace(from,"[.][m][d]$",".docx")))
  
  file.remove(to_html)
  file.remove(to_docx)
  
  return (invisible(TRUE))
}
