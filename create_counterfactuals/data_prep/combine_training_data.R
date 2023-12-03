

# import data -------------------------------------------------------------

source("./scripts/setup_colc.R")

energy_data <- readRDS(paste0(input_data_folder, "all_training_energy_data"))
climate_data <- readRDS(paste0(input_data_folder, "daily_climate.RDS"))
calendar_data <- readRDS(paste0(input_data_folder, "calendar_variables.RDS"))
participant_data <- readRDS("./data/serl_participant_summary_edition05.RDS")


# Combine -----------------------------------------------------------------

setnames(climate_data, "analysis_date", "Read_date_effective_local")
setnames(calendar_data, "date", "Read_date_effective_local")


training_data <- participant_data[, .(PUPRN, grid_cell)][energy_data, on = "PUPRN"]
training_data <- climate_data[training_data, on = c("grid_cell", 
                                                    "Read_date_effective_local")]
training_data <- calendar_data[training_data, on = c("Read_date_effective_local")]
training_data[, grid_cell := NULL]



# Reorder and save --------------------------------------------------------

setcolorder(training_data,
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



# Split into train and test set -------------------------------------------

set.seed(242)

# Give everyone the same training and test days (for elec and gas)
days <- unique(training_data$Read_date_effective_local)
days_list <- lapply(c(1:3, 10:12), function(m) {
  train_test_days <- data.table(Read_date_effective_local = days[month(days) == m])
  train_test_days[, rand := rnorm(nrow(train_test_days))]
  setkey(train_test_days, rand)
  train_test_days[, test := FALSE]
  train_test_days[1:test_days_per_month, test := TRUE]
})
train_test_days <- rbindlist(days_list)

training_data_split <- train_test_days[, .(Read_date_effective_local, test)][training_data,
                                                                             on = "Read_date_effective_local"]



# Create separate fuel data lists -----------------------------------------

# Electricity
elec_id <- sort(unique(training_data_split[fuel == "elec", PUPRN]))
elec_training_list <- lapply(elec_id, function(puprn) {
  return(training_data_split[fuel == "elec" & PUPRN == puprn])
})

# Gas
gas_id <- sort(unique(training_data_split[fuel == "gas", PUPRN]))
gas_training_list <- lapply(gas_id, function(puprn) {
  return(training_data_split[fuel == "gas" & PUPRN == puprn])
})



# Save --------------------------------------------------------------------


saveRDS(elec_id, elec_id_file)
saveRDS(elec_training_list, elec_training_data_file)


saveRDS(gas_id, gas_id_file)
saveRDS(gas_training_list, gas_training_data_file)
