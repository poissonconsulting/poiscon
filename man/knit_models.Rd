\name{knit_models}
\alias{knit_models}
\title{Knit models}
\usage{
knit_models(replacement)
}
\arguments{
  \item{replacement}{an optional named character vector
  which specifies replacement names for directories as well
  as the order to print the estimates.}
}
\value{
Knits model code and parameter descriptions.
}
\description{
Takes all models in r files in working directory with
'models-' in name and prints model code in markdown format
together with a table of a description of the parameters.
}

