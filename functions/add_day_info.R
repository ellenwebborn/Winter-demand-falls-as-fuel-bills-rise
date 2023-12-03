#' Labels days of the week, flags weekends and holidays
#' 
#' 
#' @param dt is a data.table which you're looking to add columns to
#' @param date_col is the name of column with containing the date
#' @param add_holidays (T/F) indicates if a column for bank holidays
#'   should be included
#' @param year_range gives the earliest and latest year to look for 
#'   holiday dates if add_holidays = TRUE
#'   
#' Updates dt by reference so no assignment needed when function called
#' 
#' @author Ellen Zapata-Webborn \email{e.webborn@ucl.ac.uk} (original)


add_day_info <- function(dt, 
                         date_col,
                         add_holidays = TRUE,
                         year_range = 2019:2020) {

  dt[, weekday := as.factor(weekdays(get(date_col)))]
  dt[, weekend := as.factor(weekday %in% c("Saturday", "Sunday"))]
  dt[, week := week(get(date_col))]
  dt[, yday := yday(get(date_col))]
  dt[, month := month(get(date_col))]
  
  # assign unique number to each year-week combo
  yr1 <- min(year_range)
  dt[, wkyr := ((year(get(date_col)) - yr1) * 60 + week)]
  
  # sinusoidal transformation of the day of the year
  dt_transformed <- vector(mode = "list",
                           length = length(year_range))
  for(i in 1:length(year_range)) {
    tot_days <- yday(paste(year_range[i], "-12-31", sep = ""))
    yday_transform <- sinusoidal_transform(x = 1:tot_days,
                                           x_colname = "yday",
                                           sin_colname = "sin_day_yr",
                                           cos_colname = "cos_day_yr",
                                           X = tot_days)
    dt_transformed[[i]] <- yday_transform[dt[year(get(date_col)) == year_range[i]], 
                                          on = "yday"]
  }
  dt <- rbindlist(dt_transformed)
  dt[, yday := NULL]
  
  if(add_holidays == TRUE) {
    library(timeDate)
    dt[, holiday := get(date_col) %in% as.Date(holidayLONDON(year_range))]
  }
  
  return(dt)
}
