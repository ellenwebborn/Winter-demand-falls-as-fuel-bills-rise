# setup file to store common variables for the cost-of-living crisis (CoLC) project


# libraries ---------------------------------------------------------------

library(data.table)
library(caret)
library(glmnet)
library(parallel)
library(doParallel)
library(stringr)

# Frequently changed definitions ------------------------------------------

fuel <- "gas"
output_file_suffix <- paste0(Sys.Date(), "-e.RDS")
use_best_formula <- TRUE
if(use_best_formula == FALSE) {
  formula_name <- "f3"
} # if TRUE optimal selected

algorithm = "xgb_tree"
test_days_per_month <- 5
retrain_cvrmse_min <- 10 # % minimum for retraining
find_best_formula <- TRUE
save_final_models <- TRUE
combine_train_test_errors <- TRUE

# Files -------------------------------------------------------------------

data_creation_date <- "2023_08_11"

input_data_folder <- paste0("./data/processed/", data_creation_date, "/")
if(!dir.exists(input_data_folder)) {
  dir.create(input_data_folder)
}

training_data_file <- paste0(input_data_folder, "all_elec_and_gas_training_data")
elec_training_data_file <- paste0(input_data_folder, "elec_training_data.RDS")
elec_id_file <- paste0(input_data_folder, "elec_id_data.RDS")
gas_training_data_file <- paste0(input_data_folder, "gas_training_data.RDS")
gas_id_file <- paste0(input_data_folder, "gas_id_data.RDS")

prediction_data_file <- paste0(input_data_folder, "all_elec_and_gas_prediction_data")
elec_prediction_data_file <- paste0(input_data_folder, "elec_prediction_data.RDS")
gas_prediction_data_file <- paste0(input_data_folder, "gas_prediction_data.RDS")

final_counterfactuals_folder <- "./data/outputs/counterfactuals/"
final_model_error_bias_folder <- "./data/outputs/model_error_bias/"

if(fuel == "elec") {
  counterfactual_output_folder <- paste0("./data/model_training_outputs/elec_counterfactuals/")
  model_output_folder <- paste0("./data/model_training_outputs/elec_models/")
  cv_output_folder <- paste0("./data/model_training_outputs/elec_cv/")
  model_variables_folder <- paste0("./data/model_training_outputs/elec_variables/")
  selected_modles_folder <- paste0("./data/model_training_outputs/selected_elec_models/")
} else {
  counterfactual_output_folder <- paste0("./data/model_training_outputs/gas_counterfactuals/")
  model_output_folder <- paste0("./data/model_training_outputs/gas_models/")
  cv_output_folder <- paste0("./data/model_training_outputs/gas_cv/")
  model_variables_folder <- paste0("./data/model_training_outputs/gas_variables/")
  selected_modles_folder <- paste0("./data/model_training_outputs/selected_gas_models/")
}

model_training_folder <- paste0("S:/SERL_Frontier1/EZW/data/model_training_outputs/paper_trained_models_pre_selection_",
                                fuel, "/")

retrain_folderpath <- "./data/model_training_outputs/puprn_to_retrain/"
id_to_retrain_file <- paste0(retrain_folderpath, "puprn_to_retrain_", fuel, ".RDS")
retrain_formula_file <- paste0(retrain_folderpath, "formula_to_retrain_", fuel, ".RDS")

errors_folder <- paste0("./data/model_training_outputs/errors/")
final_models_folder <- paste0("./data/model_training_outputs/paper_final_models/")



# Modelling definitions ---------------------------------------------------

kfolds <- 5

hyper_params_to_test <- expand.grid(nrounds = c(100, 200, 300),
                                    eta = c(0.05, 0.1, 0.3),
                                    gamma = 0,
                                    max_depth = c(0.5, 1, 5, 10),
                                    min_child_weight = c(1, 3, 6),
                                    subsample = 1,
                                    colsample_bytree = 1)


# Weather variables -------------------------------------------------------

orig_climate_vars <- c("2m_temperature_K",
                       "surface_solar_radiation_downwards",
                       "total_precipitation",
                       "10m_u_component_of_wind",
                       "10m_v_component_of_wind",
                       "minimum_2m_temperature_K",
                       "maximum_2m_temperature_K")

climate_vars <- c("2m_temperature_K",
                  "surface_solar_radiation_downwards",
                  "total_precipitation",
                  "min_2m_temp_K",
                  "max_2m_temp_K",
                  "10m_u_component_of_wind",
                  "10m_v_component_of_wind",
                  "wind_speed_10m",
                  "max_wind_speed")

mean_climate_vars <- c("2m_temperature_K",
                        "surface_solar_radiation_downwards",
                        "total_precipitation",
                        "wind_speed_10m")

prev_days_climate_vars <- c("2m_temperature_K",
                            "surface_solar_radiation_downwards")
  
n_prev_weather_days <- 3
