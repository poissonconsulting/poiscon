\name{cleanup_datetime}
\alias{cleanup_datetime}
\title{Cleans up datetime from data frame}
\usage{
cleanup_datetime(data, prefix = "", suffix = "", expand = c("Year",
  "Month", "Day", "Hour", "Minute", "Second"))
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
}
\value{
Cleaned up data
}
\description{
Cleans up datetime from data frame
}
\examples{
data <- data.frame(Year = 2000, Month = 1:4, Day = 2, Comments = "No way")
cleanup_datetime(data)
}
\seealso{
\code{\link{extract_datetime}}
}

