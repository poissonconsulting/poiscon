\name{extract_time}
\alias{extract_time}
\title{Extracts time from data frame}
\usage{
extract_time(data, prefix = "", suffix = "", expand = c("Hour", "Minute",
  "Second"), tz = "")
}
\arguments{
  \item{data}{data.frame with columns of
  [prefix]expand[suffix], [prefix]expand[suffix] values
  etc}

  \item{prefix}{string}

  \item{suffix}{string}

  \item{expand}{character vector of column names where
  [prefix]expand[suffix] and expand must only be Hour,
  Minute and/or Second}

  \item{tz}{string of time zone}
}
\value{
A datetime object
}
\description{
Extracts time from data frame
}
\seealso{
\link{sql_datetime} and \link{ISOdatetime}
}

