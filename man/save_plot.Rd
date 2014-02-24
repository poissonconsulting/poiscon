\name{save_plot}
\alias{save_plot}
\title{Save plot}
\usage{
save_plot(name = "plot", type = "figures", report = TRUE,
  caption = NULL, save_rows = 100, width = NULL, height = NULL)
}
\arguments{
  \item{name}{a character scalar of the name of the
  object.}

  \item{type}{a character scalar of the type of folder to
  save in.}

  \item{report}{logical scalar indicating whether the plot
  should be displayed to an analysis report.}

  \item{caption}{character scalar of caption else NULL.}

  \item{save_rows}{an integer scalar indicating the maximum
  of rows to save from the plot data as a csv file.}

  \item{width}{a numeric scalar indicating the percent of
  the page width (or if 10 or less the width of the page in
  inches). If NULL default the width is taken from the
  width of the previous gwindow.}

  \item{height}{a numeric scalar indicating the percent of
  the page height (or if 10 or less the width of the page
  in inches). If NULL default the height is taken from the
  height of the previous gwindow.}
}
\value{
Saves current plot as png file in current plots folder.
}
\description{
Saves current plot as a png file in current plots folder.
The dpi is given by option poiscon.dpi which by default is
320.
}
\seealso{
\code{\link{gwindow}}
}

