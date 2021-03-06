% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sql-datetime.r
\name{sql_datetime}
\alias{sql_datetime}
\title{SQL Datetime}
\usage{
sql_datetime(column = "DateTime", suffix = NULL, expand = c("Year",
  "Month", "Day", "Hour", "Minute", "Second"))
}
\arguments{
\item{column}{string of column name}

\item{suffix}{string of suffix to add to expand names}

\item{expand}{character vector of SQL functions to apply to column and names of columns}
}
\value{
A string of the SQL commands
}
\description{
Expands date time column in SQL query to extract Year, Month, ..., Second values.
}
\examples{

sql_datetime()
sql_datetime(suffix = 'Caught')
sql_datetime('RecaughtD8Time')
sql_datetime('DateTimeRecaught')
sql_datetime('RecaughtDateTime')

}
\seealso{
\link{sql_date} and \link{sql_time}
}
