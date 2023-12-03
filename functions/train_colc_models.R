# train counterfactual predictive model, output model and cross-validation results


train_colc_models <- function(id,
                              training_data = training_data,
                              prediction_data = prediction_data,
                              my_formula = my_formula,
                              all_formulas = all_formulas,
                              get_training_error = TRUE,
                              use_holdout = TRUE) {
  
  if(length(my_formula) == 1) {
    my_formula <- all_formulas[my_formula][[1]]
  } else {
    my_formula <- all_formulas[my_formula[id]][[1]]
  }

  if(use_holdout == TRUE) {
    train_data <- training_data[[id]][test == FALSE]
    holdout_data <- training_data[[id]][test == TRUE]
  } else {
    train_data <- training_data[[id]]
  }
  
  

# Train model -------------------------------------------------------------

  set.seed(14)
  
  if(algorithm == "xgb_tree") {
    fitted <- xgb_tree(formula = my_formula,
                       dt = train_data)
  } else if(algorithm == "xgb_linear") {
    library(xgboost)
    fitted <- xgb_linear(formula = my_formula,
                         dt = train_data)
  } else {
    library(glmnet)
    fitted <- elastic_net_regression(formula = my_formula,
                                     dt = train_data)
    }
  

# Cross-validation --------------------------------------------------------

  if(get_training_error == TRUE) {
    
    # capture cross-validation predictions
    preds <- data.table(fitted$pred)
    if(typeof(preds$pred) == "character") {
      preds[, pred := as.numeric(pred)]
    }
    
    train_data[, rowIndex := 1:nrow(train_data)]
    
    cv_predictions <- preds[, .(rowIndex,
                                pred,
                                obs)][train_data[, .(
                                  fuel,
                                  rowIndex,
                                  PUPRN,
                                  Read_date_effective_local,
                                  month,
                                  weekday,
                                  weekend,
                                  weekend_or_holiday
                                )], 
                                on = "rowIndex"]
    setnames(
      cv_predictions,
      c("pred",
        "obs"),
      c("predicted_energy_kWh_per_day",
        "observed_energy_kWh_per_day"))
    
    cv_predictions[, rowIndex := NULL]
    cv_predictions[, obs_minus_pred_kWh_per_day := observed_energy_kWh_per_day - 
                     predicted_energy_kWh_per_day]
    
    setcolorder(cv_predictions,
                c("fuel",
                  "PUPRN",
                  "Read_date_effective_local",
                  "month",
                  "weekday",
                  "weekend",
                  "weekend_or_holiday",
                  "predicted_energy_kWh_per_day",
                  "observed_energy_kWh_per_day",
                  "obs_minus_pred_kWh_per_day")) 
  }
   
  

# Predictions -------------------------------------------------------------

  # Holdout set
  if(use_holdout == TRUE) {
    predict_data <- holdout_data
    
    holdout_predictions <- predict(fitted, predict_data)
    
    holdout_predictions <- cbind(predict_data[, .(fuel, PUPRN,
                                                Read_date_effective_local,
                                                energy_kWh,
                                                month,
                                                week,
                                                weekday,
                                                weekend,
                                                weekend_or_holiday,
                                                min_2m_temp_K,
                                                `2m_temperature_K_day_mean`)],
                               data.table(predicted_energy_kWh = holdout_predictions))
    
    setnames(holdout_predictions, "energy_kWh", "observed_energy_kWh")
    
    setcolorder(holdout_predictions,
                c("fuel", "PUPRN", "Read_date_effective_local",
                  "observed_energy_kWh", "predicted_energy_kWh"))
  }
  
  
  # Counterfactuals
  if(!is.na(prediction_data)) {
    predict_data <- prediction_data[[id]]
    
    counterfactuals <- predict(fitted, predict_data)
    
    final_predictions <- cbind(predict_data[, .(fuel, PUPRN,
                                                Read_date_effective_local,
                                                energy_kWh,
                                                month,
                                                week,
                                                weekday,
                                                weekend,
                                                weekend_or_holiday,
                                                min_2m_temp_K,
                                                `2m_temperature_K_day_mean`)],
                               data.table(predicted_energy_kWh = counterfactuals))
    
    setnames(final_predictions, "energy_kWh", "observed_energy_kWh")
    
    setcolorder(final_predictions,
                c("fuel", "PUPRN", "Read_date_effective_local",
                  "observed_energy_kWh", "predicted_energy_kWh"))
  }
  

# Outputs -----------------------------------------------------------------

  to_output <- vector("list")
  
  if (get_training_error == TRUE) {
    to_output <- append(to_output, list(cross_valid_predictions = cv_predictions))
  }  
  
  if(use_holdout == TRUE) {
    to_output <- append(to_output, list(holdout_predictions = holdout_predictions))
  }
  
  if(!is.na(prediction_data)) {
    to_output <- append(to_output, list(counterfactuals = final_predictions))
  }
  
  # get the best hyperparameters
  to_output <- append(to_output, list(chosen_hyperP = fitted$bestTune))
  
  return(to_output)
  
}



