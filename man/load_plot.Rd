\name{load_plot}
\alias{load_plot}
\title{Load plot}
\usage{
  load_plot(name = "plot", type = "figures", ext = "rds")
}
\arguments{
  \item{name}{the name of the plot (without extension).}

  \item{type}{the plot type i.e. figures (default) or
  analysis, slides etc.}

  \item{ext}{the extension of the file type to read in i.e.
  ".rds" the default or ".csv" the only option currently
  implemented.}
}
\value{
  a data.frame object
}
\description{
  In current form reads in plot .csv file from current plot
  folder.
}

