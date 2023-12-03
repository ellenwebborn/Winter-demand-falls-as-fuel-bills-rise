


# setup -------------------------------------------------------------------

library(data.table)

elec_cf <- readRDS("./data/model_training_outputs/elec_counterfactuals/elec_counterfactuals.RDS")
gas_cf <- readRDS("./data/model_training_outputs/gas_counterfactuals/gas_counterfactuals.RDS")


# Format ------------------------------------------------------------------

# Round to nearest Wh as this is the accuracy of the smart meters
elec_cf[, `:=`(observed_energy_kWh = round(observed_energy_kWh, 3),
               predicted_energy_kWh = round(predicted_energy_kWh, 3),
               savings_kWh = -round(obs_min_pred_kWh, 3),
               obs_min_pred_kWh = NULL)]
gas_cf[, `:=`(observed_energy_kWh = round(observed_energy_kWh, 3),
              predicted_energy_kWh = round(predicted_energy_kWh, 3),
              savings_kWh = -round(obs_min_pred_kWh, 3),
              obs_min_pred_kWh = NULL)]


# wide to long
all_cf <- lapply(list(elec_cf, gas_cf), function(dt) {
  melt(
    dt,
    id.vars = c(
      "fuel",
      "PUPRN",
      "Read_date_effective_local",
      "month",
      "week",
      "weekday",
      "weekend",
      "weekend_or_holiday",
      "min_2m_temp_C",
      "max_2m_temp_C",
      "mean_2m_temp_C"
    ),
    measure.vars = c(
      "observed_energy_kWh",
      "predicted_energy_kWh",
      "savings_kWh"
    )
  )
})

all_cf <- rbindlist(all_cf)


# Save --------------------------------------------------------------------

saveRDS(all_cf, "./data/model_training_outputs/all_counterfactuals/all_cf_long.RDS")
