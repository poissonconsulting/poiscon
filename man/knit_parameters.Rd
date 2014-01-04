\name{knit_parameters}
\alias{knit_parameters}
\title{Knit parameters}
\usage{
knit_parameters(replacement = NULL)
}
\arguments{
  \item{replacement}{an optional named character vector
  which specifies replacement names for directories as well
  as the order to print the estimates.}
}
\value{
Prints parameter estimates in markdown format.
}
\description{
Takes all parameter estimates in tables folder and prints
in markdown format using the knitr::kable function.
}

