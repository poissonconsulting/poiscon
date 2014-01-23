\name{sql_time}
\alias{sql_time}
\title{SQL Time}
\usage{
sql_time(column = "Time", suffix = NULL, expand = c("Hour", "Minute",
  "Second"))
}
\arguments{
  \item{column}{string of column name}

  \item{suffix}{string of suffix to add to expand names}

  \item{expand}{character vector of SQL functions to apply
  to column and names of columns}
}
\value{
A string of the SQL commands
}
\description{
Expands time column in SQL query to extract Hour, Minute,
Second values.
}
\seealso{
\link{sql_datetime} and \link{sql_date}
}

