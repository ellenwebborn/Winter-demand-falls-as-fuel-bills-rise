


# setup -------------------------------------------------------------------

source("./scripts/setup_colc.R")
source("./scripts/functions/get_daily_model_errors.R")


# Load data ---------------------------------------------------------------

final_models <- readRDS(paste0(final_models_folder, fuel, "_final_models.RDS"))
selected_elec_puprn <- readRDS(paste0(final_models_folder, "elec", "_puprn_sufficiently_accurate.RDS"))
selected_gas_puprn <- readRDS(paste0(final_models_folder, "gas", "_puprn_sufficiently_accurate.RDS"))


# Handle post-training participant filtering
pred_data <- readRDS( paste0(input_data_folder, "all_prediction_energy_data"))

orig_elec_id <- unique(pred_data[fuel == "elec", PUPRN])
selected_elec_id <- intersect(selected_elec_puprn, orig_elec_id)

orig_gas_id <- unique(pred_data[fuel == "gas", PUPRN])
selected_gas_id <- intersect(selected_gas_puprn, orig_gas_id)

# Only keep households selected for the elec and gas samples 
#  (to make pieces of analysis comparable and to simplify the narrative in the paper)

selected_id <- intersect(selected_elec_id, selected_gas_id)

if(fuel == "elec") {
  final_models <- final_models[fuel == "elec" & 
                                 PUPRN %in% selected_id]
  saveRDS(selected_id, paste0(final_models_folder, fuel, "_puprn_sufficiently_accurate.RDS"))
  
} else {
  final_models <- final_models[fuel == "gas" & 
                                       PUPRN %in% selected_id]
  saveRDS(selected_id, paste0(final_models_folder, fuel, "_puprn_sufficiently_accurate.RDS"))
}

saveRDS(final_models, paste0(final_models_folder, fuel, "_final_models.RDS"))


# Load all training data
# load all trained models 
files <- list.files(model_training_folder)
n_files <- length(files)

trained_models <- lapply(files, function(f) {
  readRDS(paste0(model_training_folder, f))
})

# Get cross validation data
# Extract cross validation data
cross_valids <- lapply(1:n_files, function(i) {
  cv <- rbindlist(lapply(trained_models[[i]][1:(lengths(trained_models)[i] - 4)], '[[',
                         "cross_valid_predictions"))
  return(cbind(data.table(model_id = rep(i, nrow(cv))),
               cv))
})
cross_valids <- rbindlist(cross_valids)

# Get holdout data
holdouts <- lapply(1:n_files, function(i) {
  cv <- rbindlist(lapply(trained_models[[i]][1:(lengths(trained_models)[i] - 4)], '[[',
                         "holdout_predictions"))
  return(cbind(data.table(model_id = rep(i, nrow(cv))),
               cv))
  
})
holdouts <- rbindlist(holdouts)

# Filter
final_models[, final := TRUE]
final_cross_valids <- final_models[cross_valids, on = .(PUPRN, model_id, fuel)]
final_cross_valids <- final_cross_valids[final == TRUE]

final_holdouts <- final_models[holdouts, on = .(PUPRN, model_id, fuel)]
final_holdouts <- final_holdouts[final == TRUE]


# Caculate train and test errors --------------------------------------------------

train_errors <- get_daily_model_errors(final_cross_valids,
                                       "observed_energy_kWh_per_day",
                                       "predicted_energy_kWh_per_day",
                                       "train")
train_errors

test_errors <- get_daily_model_errors(final_holdouts,
                                      "observed_energy_kWh",
                                      "predicted_energy_kWh",
                                      "test")
train_and_test_errors <- rbind(train_errors, test_errors)
train_and_test_errors[, n_households := final_models[, length(unique(PUPRN))]]
train_and_test_errors

# Save --------------------------------------------------------------------

saveRDS(train_and_test_errors,
        paste0(errors_folder, fuel, "train_test_errors.RDS"))



if(combine_train_test_errors == TRUE) {
  if(fuel == "elec" & 
     file.exists(paste0(errors_folder, "gas", "train_test_errors.RDS"))) {
    other_fuel_train_test <- readRDS(paste0(errors_folder, "gas", "train_test_errors.RDS"))
  } else if(fuel == "gas" & 
          file.exists(paste0(errors_folder, "elec", "train_test_errors.RDS"))) {
    other_fuel_train_test <- readRDS(paste0(errors_folder, "elec", "train_test_errors.RDS"))
  } 
  if(exists("other_fuel_train_test")) {
    elec_gas_train_test <- rbind(train_and_test_errors,
                                 other_fuel_train_test)
    saveRDS(elec_gas_train_test,
            paste0(errors_folder,"elec_and_gas_train_test_errors.RDS"))
    fwrite(elec_gas_train_test,
           file = "./data/outputs/model_info_errors/elec_and_gas_train_test_errors.csv")
  }
}
  
# Save final models summary
if(fuel == "elec") {
  final_elec_models_summary <- final_models[fuel == "elec", .N, keyby = .(model_id, formula)]
  final_elec_models_summary[, fuel := "elec"] 
  saveRDS(final_elec_models_summary,
          paste0(errors_folder, fuel, "final_elec_models_summary.RDS"))
  fwrite(final_elec_models_summary, 
         file = "./data/outputs/model_info_errors/elec_models_summary.csv")
} else {
  final_gas_models_summary <- final_models[fuel == "gas", .N, keyby = .(model_id, formula)]
  final_gas_models_summary[, fuel := "gas"] 
  saveRDS(final_gas_models_summary,
          paste0(errors_folder, fuel, "final_gas_models_summary.RDS"))
  fwrite(final_gas_models_summary, 
         file = "./data/outputs/model_info_errors/gas_models_summary.csv")
}

  