#' Return daily summary variables from hourly climate data
#'  
#' Outputs a data table with grid_cell, analysis_date,
#'   and the mean of each climate variable specified for the date in the row, 
#'   and the 3 previous days
#' 
#' 
#' @param vars is a vector of strings indicating climate variables e.g
#'   "2m_temperature_K"
#' @param prev_days_vars a vector of strings indicating climate variables to include for previous days 
#' @param climate is the climate data table
#' @param n_prev_days number of previous days to include in the model
#'   
#' @author Ellen Zapata-Webborn \email{e.webborn@ucl.ac.uk} (original)



get_daily_weather_means <- function(vars = climate_variables,
                                    prev_vars = prev_days_vars,
                                    climate_data = climate,
                                    n_prev_days = 3) {
  
  if(!"analysis_date" %in% colnames(climate_data)) {
    climate_data[, analysis_date := date(Read_date_time_local)]
  }

  daily_means <- climate_data[, lapply(.SD, mean), .SDcols = vars,
                              by = c("grid_cell", 
                                     "analysis_date")]
  

  if(n_prev_days > 0) {
    
    prev <- vector(mode = "list", length = n_prev_days)
    
    for(i in 1:n_prev_days) {
      prev[[i]] <- copy(daily_means[, .SD, .SDcols = c("grid_cell",
                                                       "analysis_date",
                                                       prev_vars)])
      setnames(prev[[i]],
               old = prev_vars,
               new = paste(prev_vars, "_day_mean_prev_", i, sep = ""))
      prev[[i]][, analysis_date := analysis_date + i]
      
    }
  }
  
  setnames(daily_means,
           vars,
           paste(vars, "_day_mean", sep = ""))
  
  setkeyv(daily_means, 
          c("grid_cell", "analysis_date"))
  
  if(n_prev_days > 0) {
    for(i in 1:n_prev_days) {
      setkeyv(prev[[i]], 
              c("grid_cell", "analysis_date"))
      
      daily_means <- prev[[i]][daily_means]
    }
  }
  
  return(daily_means)
}