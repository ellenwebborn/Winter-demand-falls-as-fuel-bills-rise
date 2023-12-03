# file to store and save formulas for regression

folder <- "./scripts/regression_formulas/"


f0 <- as.formula("energy_kWh ~ 
    sin_day_yr + cos_day_yr + weekend_or_holiday +
    `2m_temperature_K_day_mean` +
    surface_solar_radiation_downwards_day_mean +
    total_precipitation_day_mean +
    wind_speed_10m_day_mean")

saveRDS(f0, paste0(folder, "f0.RDS"))

f1 <-
  as.formula(
    "energy_kWh ~ 
    sin_day_yr + cos_day_yr + weekend_or_holiday +
    `2m_temperature_K_day_mean_prev_1` + `2m_temperature_K_day_mean` +
    max_2m_temp_K + min_2m_temp_K +
    surface_solar_radiation_downwards_day_mean_prev_1 + 
    surface_solar_radiation_downwards_day_mean +
    total_precipitation_day_mean +
    max_wind_speed + wind_speed_10m_day_mean") 
                
saveRDS(f1, paste0(folder, "f1.RDS"))

f2 <-
  as.formula(
    "energy_kWh ~ 
    sin_day_yr + cos_day_yr + weekend_or_holiday + 
    weekend_or_holiday * `2m_temperature_K_day_mean_prev_1` + 
    weekend_or_holiday * `2m_temperature_K_day_mean` +
    weekend_or_holiday * max_2m_temp_K + 
    weekend_or_holiday * min_2m_temp_K +
    weekend_or_holiday * surface_solar_radiation_downwards_day_mean_prev_1 + 
    weekend_or_holiday * surface_solar_radiation_downwards_day_mean +
    weekend_or_holiday * total_precipitation_day_mean +
    weekend_or_holiday * max_wind_speed + 
    weekend_or_holiday * wind_speed_10m_day_mean") 

saveRDS(f2, paste0(folder, "f2.RDS"))


f3 <-
  as.formula(
    "energy_kWh ~ 
    sin_day_yr + cos_day_yr + weekend_or_holiday + 
    `2m_temperature_K_day_mean` +
    min_2m_temp_K +
    surface_solar_radiation_downwards_day_mean +
    total_precipitation_day_mean +
    max_wind_speed + 
    wind_speed_10m_day_mean +
    weekend_or_holiday * `2m_temperature_K_day_mean` +
    weekend_or_holiday * min_2m_temp_K +
    weekend_or_holiday * surface_solar_radiation_downwards_day_mean +
    weekend_or_holiday * total_precipitation_day_mean +
    weekend_or_holiday * max_wind_speed + 
    weekend_or_holiday * wind_speed_10m_day_mean") 

saveRDS(f3, paste0(folder, "f3.RDS"))
