#' @title Cleans up datetime from data frame
#' 
#' @param data data.frame with columns of [prefix]expand[suffix], [prefix]expand[suffix] values etc
#' @param prefix string
#' @param suffix string
#' @param expand character vector of column names where [prefix]expand[suffix]
#' and expand must only be Year, Month, Day, Hour, Minute and/or Second
#' @return Cleaned up data
#' @seealso \code{\link{extract_datetime}}
#' @examples
#' data <- data.frame(Year = 2000, Month = 1:4, Day = 2)
#' cleanup_datetime(data)
#' data <- data.frame(Year = 2000, Month = 1:4, Day = 2, Comments = "No way")
#' cleanup_datetime(data)
#' @export
cleanup_datetime <- function (data, prefix = "", suffix = "",
                              expand = c("Year", "Month", "Day", "Hour", "Minute", "Second")) {
  
  assert_that(is.string(prefix) && noNA(prefix))
  assert_that(is.string(suffix) && noNA(suffix))
  assert_that(is.character(expand) && noNA(expand))
  assert_that(all(expand %in% c("Year", "Month", "Day", "Hour", "Minute", "Second")))
  assert_that(!any(duplicated(expand)))
  
  for (x in expand) {
    column <- paste0(prefix, x, suffix)
    if(!column %in% colnames(data)) {
      warning("Column '", column, "' not in data")
    } else
      data[[column]] <- NULL
  }
  data
}

#' @title Cleans up date from data frame
#' 
#' @param data data.frame with columns of [prefix]expand[suffix], [prefix]expand[suffix] values etc
#' @param prefix string
#' @param suffix string
#' @param expand character vector of column names where [prefix]expand[suffix]
#' and expand must only be Year, Month, Day
#' @return Cleaned up data
#' @seealso \code{\link{cleanup_datetime}}
#' @examples
#' data <- data.frame(Year = 2000, Month = 1:4, Day = 2, Comments = "No way")
#' cleanup_date(data)
#' @export
cleanup_date <- function (data, prefix = "", suffix = "",
                          expand = c("Year", "Month", "Day")) {
  
  assert_that(is.character(expand) && noNA(expand))
  assert_that(all(expand %in% c("Year", "Month", "Day")))
  assert_that(!any(duplicated(expand)))
  
  cleanup_datetime(data, prefix, suffix, expand)
}

#' @title Cleans up time from data frame
#' 
#' @param data data.frame with columns of [prefix]expand[suffix], [prefix]expand[suffix] values etc
#' @param prefix string
#' @param suffix string
#' @param expand character vector of column names where [prefix]expand[suffix]
#' and expand must only be Hour, Minute and/or Second
#' @return Cleaned up data
#' @seealso \code{\link{cleanup_datetime}}
#' @export
cleanup_time <- function (data, prefix = "", suffix = "",
                          expand = c("Hour", "Minute", "Second")) {
  
  assert_that(is.character(expand) && noNA(expand))
  assert_that(all(expand %in% c("Hour", "Minute", "Second")))
  assert_that(!any(duplicated(expand)))
  
  cleanup_datetime(data, prefix, suffix, expand)
}
