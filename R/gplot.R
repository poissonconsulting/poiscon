gplot <- function (ggplot, width, height, dpi, report, caption) {
  
  assert_that(is.ggplot(ggplot))
  assert_that(is.number(width))
  assert_that(is.number(height))
  assert_that(is.count(dpi))
  assert_that(is.flag(report) && noNA(report))
  assert_that(is.null(caption) || is.string(caption))
  
  object <- list(ggplot = ggplot, width = width, height = height, 
                 dpi = dpi, report = report, caption = caption)
  
  class(object) <- "gplot"
  
  object
}

is.gplot <- function (object) {
  inherits(object, "gplot")
}
