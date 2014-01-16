\name{figures_web}
\alias{figures_web}
\title{Extract figures}
\usage{
figures_web(dir = "poissonconsulting.github.io")
}
\arguments{
  \item{dir}{jekyll repository}
}
\value{
An invisible logical scalar indicating whether or not
sucessful.
}
\description{
Extracts all the png files from output/plots/figures and
saves to output/report/project-name/figures ready for
linking to the report and posting on the jekyll site in the
figures directory.
}

