

#' @export
p <- function (x, ... ) {
  return (base::print (x, ...))
}

#' @export
s <- function (object, ... ) {
  return (base::summary (object, ...))
}

#' @export
h <- function (x, ... ) {
  return (utils::head (x, ...))
}

#' @export
ht <- function (x, ...) {
  return (list(head = head (x, ...), tail = tail(x, ...)))
}
