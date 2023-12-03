# Function that compares the model accuracy with our threshold for retraining the model
#  and returns PUPRN that haven't been trained yet or don't have good model accuracy

check_if_retraining_needed <- function(model_accuracy = selected_models,
                                       id = id) {
  model_accuracy[, retrain := cv_rmse >= retrain_cvrmse_min]
  id_to_retrain <- data.table(PUPRN = id)
  id_to_retrain <- model_accuracy[, .(PUPRN, retrain)][id_to_retrain, on = "PUPRN"]
  id_to_retrain <- id_to_retrain[retrain != FALSE | is.na(retrain)]$PUPRN
}
