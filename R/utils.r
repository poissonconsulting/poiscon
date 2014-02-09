
#' @title Print
#'
#' @description
#' Short hand for print
#' 
#' @param x an object to print.
#' @param ... further arguments passed to or from other methods.
#' @return Prints x.
#' @seealso \code{\link{s}}, \code{\link{h}}, \code{\link{ht}}.
#' @export
p <- function (x, ... ) {
  print (x, ...)
}

#' @title Summary
#'
#' @description
#' Short hand for summary
#' 
#' @param object an object to summarise
#' @param ... further arguments passed to or from other methods.
#' @return summarises x.
#' @seealso \code{\link{p}}, \code{\link{h}}, \code{\link{ht}}.
#' @export
s <- function (object, ... ) {
  summary(object, ...)
}

#' @title Head
#'
#' @description
#' Short hand for head
#' 
#' @param x an object to get the head
#' @param ... further arguments passed to or from other methods.
#' @return heads x.
#' @seealso \code{\link{p}}, \code{\link{s}}, \code{\link{ht}}.
#' @export
h <- function (x, ... ) {
  head(x, ...)
}

#' @title Head-tail
#'
#' @description
#' Short hand for head and tail
#' 
#' @param x an object to get the head and tail
#' @return Head and tail of x.
#' @seealso \code{\link{p}}, \code{\link{s}}, \code{\link{h}}.
#' @export
ht <- function (x) {
  list(head = h(x), tail = t(x))
}

#' @title Print Summary
#'
#' @description
#' Short hand for print summary
#' 
#' @param x an object to summarise and print
#' @return summarises x.
#' @seealso \code{\link{p}} and \code{\link{s}}.
#' @export
ps <- function (x) {
  p(s(x))
}

#' @title Print Head
#'
#' @description
#' Short hand for print head
#' 
#' @param x an object to print the head
#' @return heads x.
#' @seealso \code{\link{p}} and \code{\link{h}}.
#' @export
ph <- function (x) {
  p(h(x))
}

#' @title Head-tail
#'
#' @description
#' Short hand for head and tail
#' 
#' @param x an object to print the head and tail
#' @return Print head and tail of x.
#' @seealso \code{\link{p}} and \code{\link{ht}}.
#' @export
pht <- function (x) {
  p(ht(x))
}
