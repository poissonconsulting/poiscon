\name{save_plot}
\alias{save_plot}
\title{Save plot}
\usage{
save_plot(name = "plot", type = "figures", dwidth = NULL,
  dheight = NULL, slide = NULL, saveTable = TRUE, report = TRUE,
  caption = NULL)
}
\arguments{
  \item{name}{a character scalar of the name of the
  object.}

  \item{type}{a character scalar of the type of folder to
  save in.}

  \item{dwidth}{a numeric scalar indicating the proportion
  of the window width. By default NULL and taken from plot
  window.}

  \item{dheight}{a numeric scalar indicating the proportion
  of the window height. By default NULL and taken from plot
  window.}

  \item{slide}{a logical scalar or NULL indicating whether
  a slide window.}

  \item{saveTable}{a logical scalar indicating whether to
  save the plot data.frame as a csv file.}

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

