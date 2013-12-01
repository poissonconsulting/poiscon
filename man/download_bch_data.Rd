\name{download_bch_data}
\alias{download_bch_data}
\title{Download BCH data}
\usage{
  download_bch_data(code = "DDM", period = "hourly",
    startDate = as.Date("2013-01-01"),
    endDate = Sys.Date(), status = "questionable",
    surrogate = "questionable", add_spaces = TRUE,
    access_file = "../../../bchydro_data_12/data/based/download/ColDuncEnv.accdb")
}
\arguments{
  \item{code}{a character vector of the station codes.}

  \item{period}{a character element of the period by
  default "hourly" but can also be "raw" and "daily".}

  \item{startDate}{a Date element of the beginning of the
  time series.}

  \item{endDate}{a Date element of the end of the time
  series. By default the current date.}

  \item{status}{a character scalar indicating which type of
  values should be included.}

  \item{surrogate}{a character scalar indicating which type
  of values should be repalced by a surrogate if
  available.}

  \item{add_spaces}{a logical scalar indicating whether
  missing values should be filled in with NAs.}

  \item{access_file}{an character scalar indicating the
  path to the database.}
}
\value{
  A data.frame object.
}
\description{
  Download data from environmental database
}
\seealso{
  \code{\link{dayte}}.
}

