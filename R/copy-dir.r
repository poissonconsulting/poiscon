# inspired by similar function in slidify
copy_dir <- function (from, to) 
{
  assert_that(is.string(from))
  assert_that(is.string(to))
  
  if (!(file.exists(to)))
    dir.create(to, recursive = TRUE)
  
  message("Copying files to ", to, "...")
  return (invisible(file.copy(from = list.files(from, full.names = T), 
                              to, recursive = TRUE)))
}
