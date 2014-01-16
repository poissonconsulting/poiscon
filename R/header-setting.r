header_setting <- function (file, setting = "title") {
  
  assert_that(is.string(file))
  assert_that(file.exists(file))
  
  if(substr(file, nchar(file)-2, nchar(file)) != ".md")
    stop("file must be a .md file")
  
  text <- readLines(file)
  if(text[1] != "---")
    stop("header must start with ---")
  
  index <- grep(paste0("^", setting,"[:][ ]."), text, )
  
  if(length(index) != 1)
    return (NULL)
  text <- text[index]
  text <- str_replace(text, paste0("^", setting,"[:][ ]"), "")
  return (text)
}
