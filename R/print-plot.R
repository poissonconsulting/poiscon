#' Print Plot
#'
#' Function to use an print_plot(gp) in place of print(gp) as catches error and retries.
#'
#' @param x The plot object to plot.
#' @export
#'
#' @examples
print_plot <- function (x) {
  if (inherits(try(print(x), silent = TRUE), "try-error"))
    try(print(x))
  invisible(x)
}
