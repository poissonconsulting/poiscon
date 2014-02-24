\name{resave_plots}
\alias{resave_plots}
\title{Resave plots}
\usage{
resave_plots(type = "figures", dpi = NULL)
}
\arguments{
  \item{type}{a character element indicating the type of
  plots to redraw by default type = "figures"}

  \item{dpi}{integer scalar of dots per inch. If NULL (the
  default) then given by option poiscon.dpi which by
  default is 320}
}
\value{
invisible logical scalar indicating whether successful.
}
\description{
Can be used to resave plots with different theme and/or if
in windows to ensure correct height and width when inserted
into Microsoft Word document
}

