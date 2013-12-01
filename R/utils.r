
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
  return (base::print (x, ...))
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
  return (base::summary (object, ...))
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
  return (utils::head (x, ...))
}

#' @title Head-tail
#'
#' @description
#' Short hand for head and tail
#' 
#' @param x an object to get the head and tail
#' @param ... further arguments passed to or from other methods.
#' @return Head and tail of x.
#' @seealso \code{\link{p}}, \code{\link{s}}, \code{\link{h}}.
#' @export
ht <- function (x, ...) {
  return (list(head = head (x, ...), tail = tail(x, ...)))
}
