rm_blank_lines_top <- function (file) {
  
  assert_that(is.string(file))
  assert_that(file.exists(file))
  
  text <- readLines(file)
  if(all(text == ""))
    stop("file is blank")
  
  while(text[1] == "")
    text <- text[-1]
  writeLines(text, file)
  return (invisible())
}
