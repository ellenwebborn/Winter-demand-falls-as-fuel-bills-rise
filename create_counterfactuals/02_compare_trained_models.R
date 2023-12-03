

# setup -------------------------------------------------------------------

source("./scripts/setup_colc.R")
source("./scripts/functions/check_if_retraining_needed.R")


# Load model outputs, extract variables -----------------------------------


# load all trained models 
files <- list.files(model_training_folder)
n_files <- length(files)

trained_models <- lapply(files, function(f) {
  readRDS(paste0(model_training_folder, f))
})


# Extract cross validation data
cross_valids <- lapply(1:n_files, function(i) {
  cv <- rbindlist(lapply(trained_models[[i]][1:(lengths(trained_models)[i] - 4)], '[[',
                         "cross_valid_predictions"))
  return(cbind(data.table(model_id = rep(i, nrow(cv))),
               cv))
})
cross_valids <- rbindlist(cross_valids)

all_puprn <- unique(cross_valids$PUPRN)


# Extract chosen hyperparameters
chosen_hyperparams <- lapply(1:n_files, function(i) {
  all_puprn <- unique(cross_valids[model_id == i, PUPRN])
  hp <- rbindlist(lapply(trained_models[[i]][1:(lengths(trained_models)[i] - 4)], '[[',
                         "chosen_hyperP"))
  return(cbind(data.table(model_id = rep(i, nrow(hp)),
                          PUPRN = all_puprn[1:(lengths(trained_models)[i] - 4)],
                          algorithm = rep(trained_models[[i]]$algorithm, nrow(hp)),
                          formula = rep(trained_models[[i]]$formula, nrow(hp)),
                          hp)))
  
})
chosen_hyperparams <- rbindlist(chosen_hyperparams)


# Calculate training errors -----------------------------------------------

# get monthly data
monthly_cross_valids <- cross_valids[, .(obs = mean(observed_energy_kWh_per_day),
                                         pred = mean(predicted_energy_kWh_per_day),
                                         obs_min_pred = mean(observed_energy_kWh_per_day) - 
                                           mean(predicted_energy_kWh_per_day)),
                                     keyby = .(model_id, fuel, PUPRN, month)]

# errors for model selection
monthly_train_errors <- monthly_cross_valids[, .(cv_rmse = 1 / mean(obs) * sqrt(sum(obs_min_pred^2) / 5) * 100,
                                                 nmbe = 1 / mean(obs) * sum(obs_min_pred) / 5 * 100),
                                             keyby = .(PUPRN, model_id)]


# indicate if we should exclude based on high bias
monthly_train_errors[, nmbe_exclude := ifelse(abs(nmbe) > 5, TRUE, FALSE)]


# Select optimal models ---------------------------------------------------

selected_models <- copy(monthly_train_errors[nmbe_exclude == FALSE])
setorder(selected_models, cols = "cv_rmse")
selected_models <- unique(selected_models, by = "PUPRN")
selected_models[, fuel := fuel]

final_models <- chosen_hyperparams[selected_models, 
                                   on = c("PUPRN", "model_id")]

puprn_for_retraining <- check_if_retraining_needed(final_models, id = all_puprn)
length(puprn_for_retraining)



# Save best formulas ------------------------------------------------------

# Only need to run once - once we have the best formula, don't run again
if(find_best_formula == TRUE) {
  setorderv(monthly_train_errors, c("PUPRN", "nmbe_exclude", "cv_rmse"))
  best_formulas <- unique(monthly_train_errors, by = "PUPRN")
  best_formulas <- chosen_hyperparams[, .(PUPRN, model_id, formula)][best_formulas, 
                                                                     on = c("PUPRN", "model_id")]
  saveRDS(best_formulas[, .(PUPRN, formula)], retrain_formula_file)
}



# Replace the 'optimal' formulas in final_models with actual formulas -----

if(save_final_models == TRUE) {
  if(!exists("best_formulas")) {
    best_formulas <- readRDS(retrain_formula_file)
  }
  final_models <- final_models[cv_rmse < 15]
  final_models[, formula := NULL]
  final_models <- best_formulas[final_models, on = "PUPRN"]
}


# Save outputs ------------------------------------------------------------

# save the puprn for retraining 
saveRDS(puprn_for_retraining, id_to_retrain_file)

if(save_final_models == TRUE) {
  saveRDS(final_models,
          paste0(final_models_folder, fuel, "_final_models.RDS"))
  
  selected_puprn <- final_models$PUPRN
  saveRDS(selected_puprn,
          paste0(final_models_folder, fuel, "_puprn_sufficiently_accurate.RDS"))
}
