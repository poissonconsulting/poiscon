str_replace_file <- function (file, pattern, replacement) {
  
  assert_that(is.string(file))
  assert_that(file.exists(file))
  
  text <- readLines(file)
  text <- str_replace_all(text, pattern, replacement)
  writeLines(text, file)
  return (invisible())
}
