#' @title SQL Datetime
#'
#' @description
#' Expands date time column in SQL query to extract Year, Month, ..., Second values.
#' 
#' @param column string of column name
#' @param suffix string of suffix to add to expand names
#' @param expand character vector of SQL functions to apply to column and names of columns
#' @return A string of the SQL commands
#' @seealso \link{sql_date} and \link{sql_time}
#' @examples
#' 
#' sql_datetime()
#' sql_datetime(suffix = 'Caught')
#' sql_datetime('RecaughtD8Time')
#' sql_datetime('DateTimeRecaught')
#' sql_datetime('RecaughtDateTime')
#' 
#' @importFrom stringr str_replace
#' @export
sql_datetime <- function(column = "DateTime", suffix = NULL, expand = c("Year", "Month", 
  "Day", "Hour", "Minute", "Second")) {
  
  assert_that(is.string(column) && noNA(column))
  assert_that(is.null(suffix) || (is.string(suffix) && noNA(suffix)))
  assert_that(is.character(expand) && noNA(expand))
  assert_that(all(expand %in% c("Year", "Month", "Day", "Hour", "Minute", "Second")))
  assert_that(!any(duplicated(expand)))
  
  if (is.null(suffix)) {
    suffix <- str_replace(column, "DateTime", "")
    if (identical(suffix, column)) 
      suffix <- ""
  }
  
  paste0(expand, "(", column, ") ", "AS ", expand, suffix, collapse = ", ")
}

#' @title SQL Date
#'
#' @description
#' Expands date column in SQL query to extract Year, Month, Day values.
#' 
#' @param column string of column name
#' @param suffix string of suffix to add to expand names
#' @param expand character vector of SQL functions to apply to column and names of columns
#' @return A string of the SQL commands
#' @seealso \link{sql_datetime} and \link{sql_time}
#' 
#' @export
sql_date <- function(column = "Date", suffix = NULL, expand = c("Year", "Month", 
  "Day")) {
  
  assert_that(is.string(column) && noNA(column))
  assert_that(is.null(suffix) || (is.string(suffix) && noNA(suffix)))
  
  if (is.null(suffix)) {
    suffix <- str_replace(column, "Date", "")
    if (identical(suffix, column)) 
      suffix <- ""
  }
  
  sql_datetime(column, suffix, expand)
}

#' @title SQL Time
#'
#' @description
#' Expands time column in SQL query to extract Hour, Minute, Second values.
#' 
#' @param column string of column name
#' @param suffix string of suffix to add to expand names
#' @param expand character vector of SQL functions to apply to column and names of columns
#' @return A string of the SQL commands
#' @seealso \link{sql_datetime} and \link{sql_date}
#' 
#' @export
sql_time <- function(column = "Time", suffix = NULL, expand = c("Hour", "Minute", 
  "Second")) {
  assert_that(is.string(column) && noNA(column))
  assert_that(is.null(suffix) || (is.string(suffix) && noNA(suffix)))
  
  if (is.null(suffix)) {
    suffix <- str_replace(column, "Time", "")
    if (identical(suffix, column)) 
      suffix <- ""
  }
  
  sql_datetime(column, suffix, expand)
} 
