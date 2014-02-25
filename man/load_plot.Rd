\name{load_plot}
\alias{load_plot}
\title{Load plot}
\usage{
load_plot(name = "plot", type = "figures", dataset = FALSE)
}
\arguments{
  \item{name}{character scalar of the name of the plot
  (without extension).}

  \item{type}{character scalar indicating the plot type
  i.e. figures (default) or analysis, data etc.}

  \item{dataset}{logical scalar indicating whether to
  return the data as opposed to the ggplot object}
}
\value{
the gggplot object or the plot data
}
\description{
Reads in ggplot object
}

