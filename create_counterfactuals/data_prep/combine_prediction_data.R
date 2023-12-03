
# Setup and data loading --------------------------------------------------

source("./scripts/setup_colc.R")

# import input data
energy_data <- readRDS(paste0(input_data_folder, "all_prediction_energy_data"))
climate_data <- readRDS(paste0(input_data_folder, "daily_climate.RDS"))
calendar_data <- readRDS(paste0(input_data_folder, "calendar_variables.RDS"))
participant_data <- readRDS("./data/serl_participant_summary_edition06.RDS")



# Combine -----------------------------------------------------------------

setnames(climate_data, "analysis_date", "Read_date_effective_local")
setnames(calendar_data, "date", "Read_date_effective_local")


prediction_data <- participant_data[, .(PUPRN, grid_cell)][energy_data, on = "PUPRN"]
prediction_data <- climate_data[prediction_data, on = c("grid_cell", 
                                                    "Read_date_effective_local")]
prediction_data <- calendar_data[prediction_data, on = c("Read_date_effective_local")]

prediction_data[, grid_cell := NULL]

# Reorder and save --------------------------------------------------------

setcolorder(prediction_data,
            c("fuel",
              "PUPRN",
              "Read_date_effective_local",
              "energy_kWh",
              "month",
              "week",
              "weekday",
              "weekend",
              "holiday",
              "weekend_or_holiday",
              "sin_day_yr",
              "cos_day_yr",
              "min_2m_temp_K",
              "max_2m_temp_K",
              "2m_temperature_K_day_mean",
              "2m_temperature_K_day_mean_prev_1",
              "2m_temperature_K_day_mean_prev_2",
              "2m_temperature_K_day_mean_prev_3",
              "surface_solar_radiation_downwards_day_mean",
              "surface_solar_radiation_downwards_day_mean_prev_1",
              "surface_solar_radiation_downwards_day_mean_prev_2",
              "surface_solar_radiation_downwards_day_mean_prev_3",
              "total_precipitation_day_mean",
              "max_wind_speed",
              "wind_speed_10m_day_mean"))


saveRDS(prediction_data, 
        paste0(prediction_data_file, ".RDS"))

fwrite(prediction_data,
       paste0(prediction_data_file, ".csv"))


# Create separate fuel data lists -----------------------------------------

# Electricity

elec_id <- sort(unique(prediction_data[fuel == "elec", PUPRN]))

elec_prediction_list <- lapply(elec_id, function(puprn) {
  return(prediction_data[fuel == "elec" & PUPRN == puprn])
})

saveRDS(elec_prediction_list, elec_prediction_data_file)

# Gas

gas_id <- sort(unique(prediction_data[fuel == "gas", PUPRN]))

gas_prediction_list <- lapply(gas_id, function(puprn) {
  return(prediction_data[fuel == "gas" & PUPRN == puprn])
})

saveRDS(gas_prediction_list, gas_prediction_data_file)

