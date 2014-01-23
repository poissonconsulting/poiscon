\name{extract_datetime}
\alias{extract_datetime}
\title{Extracts datetime from data frame}
\usage{
extract_datetime(data, prefix = "", suffix = "", expand = c("Year",
  "Month", "Day", "Hour", "Minute", "Second"), tz = "")
}
\arguments{
  \item{data}{data.frame with columns of
  [prefix]expand[suffix], [prefix]expand[suffix] values
  etc}

  \item{prefix}{string}

  \item{suffix}{string}

  \item{expand}{character vector of column names where
  [prefix]expand[suffix] and expand must only be Year,
  Month, Day, Hour, Minute and/or Second}

  \item{tz}{string of time zone}
}
\value{
A date time object
}
\description{
Extracts datetime from data frame
}
\examples{
data <- data.frame(Year = 2000, Month = 1:4, Day = 2)
extract_datetime(data)
extract_date(data)
extract_time(data)
}
\seealso{
\code{\link{sql_datetime}}, \code{\link{ISOdatetime}} and
\code{\link{extract_date}}
}

