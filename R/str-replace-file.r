str_replace_file <- function (file, pattern, replacement, to = NULL) {
  
  assert_that(is.string(file))
  assert_that(file.exists(file))
  assert_that(is.null(to) || is.string(to))
  
  text <- readLines(file)
  text <- str_replace_all(text, pattern, replacement)
  
  return (writeLines(text, ifelse(is.null(to), file, to)))
}
