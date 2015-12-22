#' Theme Poisson
#'
#' A gg theme for Poisson style plots.
#'
#' To use, ensure ggplot2 is installed and then call
#' \code{theme_set(theme_Poisson())}.
#'
#' @inheritParams ggplot2::ggtheme
#' @importFrom ggplot2 %+replace%
#' @return A gg theme object.
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(data = mpg, aes(x = cty, y = hwy)) +
#' facet_wrap(~manufacturer) +
#' geom_count() +
#' theme_Poisson()
theme_Poisson <- function(base_size = 12, base_family = "") {
  ggplot2::theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text         = ggplot2::element_text(size = ggplot2::rel(0.8)),
      axis.ticks        = ggplot2::element_line(colour = "black"),
      legend.key        = ggplot2::element_rect(colour = "grey80"),
      legend.title = ggplot2::element_text(size = ggplot2::rel(0.8),
                                           face = "bold", hjust = 0),
      panel.background  = ggplot2::element_rect(fill = "white", colour = NA),
      panel.border      = ggplot2::element_rect(fill = NA, colour = "black"),
      panel.grid.major  = ggplot2::element_line(colour = "grey80", size = 0.5),
      panel.grid.minor  = ggplot2::element_line(colour = "grey90", size = 0.2),
      strip.background  = ggplot2::element_rect(fill = "grey80", colour = "black")
    )
}
