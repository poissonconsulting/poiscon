#' @title Sets colors
#'
#' @description
#' Sets color palette for poiscon
#' 
#' @param values character vector of colors (by default a color blind friendly
#' palette starting with black)
#' @return invisible logical scalar indicating if successful
#' @export
set_colors <- function(values = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
  "#0072B2", "#D55E00", "#CC79A7")) {
  stopifnot(is.character(values))
  options(poiscon.colors = values)
  invisible(TRUE)
}


#' @title Gets colors
#'
#' @description
#' Gets color palette for poiscon
#' 
#' @return color palette as character vector
#' @export
get_colors <- function() {
  getOption("poiscon.colors", c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
    "#0072B2", "#D55E00", "#CC79A7"))
} 
