#' @title Extracts datetime from data frame
#' 
#' @param data data.frame with columns of [prefix]expand[suffix], [prefix]expand[suffix] values etc
#' @param prefix string
#' @param suffix string
#' @param expand character vector of column names where [prefix]expand[suffix]
#' and expand must only be Year, Month, Day, Hour, Minute and/or Second
#' @param tz string of time zone
#' @return A date time object
#' @seealso \code{\link{sql_datetime}}, \code{\link{ISOdatetime}} 
#' and \code{\link{extract_date}}
#' @examples
#' data <- data.frame(Year = 2000, Month = 1:4, Day = 2)
#' extract_datetime(data)
#' extract_date(data)
#' extract_time(data)
#' @export
extract_datetime <- function (data, prefix = "", suffix = "",
                              expand = c("Year", "Month", "Day", "Hour", "Minute", "Second"),
                              tz = "") {
  
  assert_that(is.string(prefix) && noNA(prefix))
  assert_that(is.string(suffix) && noNA(suffix))
  assert_that(is.character(expand) && noNA(expand))
  assert_that(all(expand %in% c("Year", "Month", "Day", "Hour", "Minute", "Second")))
  assert_that(!any(duplicated(expand)))
  
  values <- list()
  
  values[["Year"]] <- 2000
  values[["Month"]] <- 1
  values[["Day"]] <- 1
  values[["Hour"]] <- 0
  values[["Minute"]] <- 0
  values[["Second"]] <- 0
  
  for (x in expand) {
    column <- paste0(prefix, x, suffix)
    if(!column %in% colnames(data)) {
      warning("Column '", column, "' not in data")
    } else {
      values[[x]] <- data[[column]]
    }
  }
  
  ISOdatetime(year = values[["Year"]], 
              month = values[["Month"]], 
              day = values[["Day"]],
              hour = values[["Hour"]],
              min = values[["Minute"]], 
              sec = values[["Second"]], tz = tz)
}

#' @title Extracts Date
#' 
#' @param data data.frame with columns of [prefix]expand[suffix], [prefix]expand[suffix] values etc
#' @param prefix string
#' @param suffix string
#' @param expand character vector of column names where [prefix]expand[suffix]
#' and expand must only be Year, Month, Day
#' @return A Date object
#' @seealso \link{extract_datetime} 
#' @export
extract_date <- function (data, prefix = "", suffix = "",
                              expand = c("Year", "Month", "Day")) {
  
  assert_that(is.string(prefix) && noNA(prefix))
  assert_that(is.string(suffix) && noNA(suffix))
  assert_that(is.character(expand) && noNA(expand))
  assert_that(all(expand %in% c("Year", "Month", "Day")))
  assert_that(!any(duplicated(expand)))
  
  values <- list()
  
  values[["Year"]] <- 2000
  values[["Month"]] <- 1
  values[["Day"]] <- 1
  
  for (x in expand) {
    column <- paste0(prefix, x, suffix)
    if(!column %in% colnames(data)) {
      warning("Column '", column, "' not in data")
    } else {
      values[[x]] <- data[[column]]
    }
  }
  
  as.Date(paste(values[["Year"]], values[["Month"]], values[["Day"]], sep = "-"))
}

#' @title Extracts time from data frame
#' 
#' @param data data.frame with columns of [prefix]expand[suffix], [prefix]expand[suffix] values etc
#' @param prefix string
#' @param suffix string
#' @param expand character vector of column names where [prefix]expand[suffix]
#' and expand must only be Hour, Minute and/or Second
#' @param tz string of time zone
#' @return A datetime object
#' @seealso \link{sql_datetime} and \link{ISOdatetime}
#' @export
extract_time <- function (data, prefix = "", suffix = "",
                              expand = c("Hour", "Minute", "Second"),
                              tz = "") {
  
  assert_that(is.character(expand) && noNA(expand))
  assert_that(all(expand %in% c("Hour", "Minute", "Second")))
  assert_that(!any(duplicated(expand)))
  
  return (extract_datetime(data, prefix, suffix, expand, tz))
}
