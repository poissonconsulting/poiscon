\name{save_plot}
\alias{save_plot}
\title{Save plot}
\usage{
save_plot(name = "plot", type = "figures", save_table = 100,
  report = TRUE, caption = NULL, width = NULL, height = NULL,
  dpi = NULL)
}
\arguments{
  \item{name}{a character scalar of the name of the
  object.}

  \item{type}{a character scalar of the type of folder to
  save in.}

  \item{save_table}{a logical scalar indicating whether to
  save the plot data.frame as a csv file or a integer
  scalar indicating the maximum number of rows to save the
  csv file for.}

  \item{report}{logical scalar indicating whether the plot
  should be displayed to an analysis report.}

  \item{caption}{character scalar of caption else NULL. #'}

  \item{width}{a numeric scalar indicating the percent of
  the page width (or the number of plots per page width -
  up to 10)}

  \item{height}{a numeric scalar indicating the percent of
  the page height - by default the same percent as the
  width (or the number of plots per page width - up to
  10).}

  \item{dpi}{integer scalar of dots per inch. If NULL (the
  default) then given by option poiscon.dpi which by
  default is 300.}
}
\value{
Saves current plot as .png file in current plots folder.
}
\description{
Saves current plot as .png file in current plots folder.
}

