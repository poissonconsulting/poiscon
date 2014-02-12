\name{save_plot}
\alias{save_plot}
\title{Save plot}
\usage{
save_plot(name = "plot", type = "figures", width = NULL, height = NULL,
  saveTable = 100, report = TRUE, caption = NULL)
}
\arguments{
  \item{name}{a character scalar of the name of the
  object.}

  \item{type}{a character scalar of the type of folder to
  save in.}

  \item{width}{a numeric scalar indicating the percent of
  the page width (or the number of plots per page width -
  up to 10)}

  \item{height}{a numeric scalar indicating the percent of
  the page height - by default the same percent as the
  width (or the number of plots per page width - up to
  10).}

  \item{saveTable}{a logical scalar indicating whether to
  save the plot data.frame as a csv file or a integer
  scalar indicating the maximum number of rows to save the
  csv file for.}

  \item{report}{logical scalar indicating whether the plot
  should be displayed to an analysis report.}

  \item{caption}{character scalar of caption else NULL.}
}
\value{
Saves current plot as .png file in current plots folder.
}
\description{
Saves current plot as .png file in current plots folder.
}

