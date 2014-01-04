\name{perform_analyses}
\alias{perform_analyses}
\title{Perform analyses}
\usage{
perform_analyses(models, ..., niters = 10^3, name = "data")
}
\arguments{
  \item{models}{a jags_model to use for the analyses}

  \item{...}{optional character vectors specifying the
  directories to pull the data from (and to save to save
  the analyses to)}

  \item{niters}{a integer scalar of the number of
  iterations}

  \item{name}{a character scalar of the name of the data}
}
\value{
Performs analyses using jaggernaut
}
\description{
Run set of analyses
}

