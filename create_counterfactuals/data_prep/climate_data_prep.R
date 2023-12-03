# Prep climate data for use in cost of living crisis modelling


# Import and combine data -------------------------------------------------

source("./scripts/setup_colc.R")
source("./scripts/functions/get_daily_weather_means.R")

selected_cols <- c("grid_cell",
                   "analysis_date",
                   "date_time_utc",
                   orig_climate_vars)

climate_filenames <- list.files("./data/climate/")
orig_climate <- lapply(climate_filenames, function(X) {
  load(paste0("./data/climate/", X))
  return(climate[, ..selected_cols])
})

climate <- rbindlist(orig_climate)


# Get daily means ---------------------------------------------------------

# Get wind speed magnitude
climate[, wind_speed_10m := sqrt(`10m_u_component_of_wind`^2 + `10m_v_component_of_wind`^2)]


daily_climate <- get_daily_weather_means(vars = mean_climate_vars,
                                         prev_vars = prev_days_climate_vars,
                                         climate_data = climate,
                                         n_prev_days = n_prev_weather_days)

# deal with max and min variables
min_max_climate <- climate[, .(min_2m_temp_K = min(minimum_2m_temperature_K),
                               max_2m_temp_K = max(maximum_2m_temperature_K),
                               max_wind_speed = max(wind_speed_10m)),
                           keyby = c("grid_cell", 
                                     "analysis_date")]

daily_climate <- min_max_climate[daily_climate, on = c("grid_cell",
                                                       "analysis_date")]

saveRDS(daily_climate, 
        file = paste0(input_data_folder, "daily_climate.RDS"))
