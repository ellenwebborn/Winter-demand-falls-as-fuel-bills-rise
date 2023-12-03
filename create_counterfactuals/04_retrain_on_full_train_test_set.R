

# setup -------------------------------------------------------------------

source("./scripts/setup_colc.R")
source("./scripts/functions/xgb_tree_parallel.R")
source("./scripts/functions/unregister_dopar.R")


# load data ---------------------------------------------------------------


id <- readRDS(paste0(final_models_folder, fuel, "_puprn_sufficiently_accurate.RDS"))
final_models <- readRDS(paste0(final_models_folder, fuel, "_final_models.RDS"))

if(fuel == "elec") {
  training_data <- readRDS(elec_training_data_file)
  prediction_data <- readRDS(elec_prediction_data_file)
} else {
  training_data <- readRDS(gas_training_data_file)
  prediction_data <- readRDS(gas_prediction_data_file)
}

# Import formulas
formula_files <- list.files("./scripts/regression_formulas/")
all_formulas <- lapply(formula_files, function(f) {
  readRDS(paste0("./scripts/regression_formulas/", f))
})
names(all_formulas) <- str_remove(formula_files, ".RDS")


# Prepare data ------------------------------------------------------------

# Filter out the PUPRN that weren't selected following model evaluation

training_selected <- vector("list")
count <- 0
for(i in 1:length(training_data)) {
  if(training_data[[i]][1, PUPRN] %in% id) {
    training_selected[count + 1] <- training_data[i]
    names(training_selected)[count + 1] <- training_data[[i]][1, PUPRN]
    count <- count + 1
  }
}

prediction_selected <- vector("list")
count <- 0
for(i in 1:length(prediction_data)) {
  if(prediction_data[[i]][1, PUPRN] %in% id) {
    prediction_selected[count + 1] <- prediction_data[i]
    names(prediction_selected)[count + 1] <- prediction_data[[i]][1, PUPRN]
    count <- count + 1
  }
}

n_id <- length(id)


# Train models on full train/test dataset ---------------------------------

# Using best formula and hyperparameters from earlier model training

# Set up parallel environment
unregister_dopar()
no_cores <- detectCores(logical = TRUE)
cl <- makeCluster(no_cores - 1)
registerDoParallel(cl)
clusterEvalQ(cl, 
             {library(data.table); 
               library(caret); 
               library(xgboost); 
               library(plyr)})
clusterExport(cl, list())

run_time <- system.time(fitted <- c(
  parLapply(
    cl,
    id,
    dt = training_selected,
    fun = xgb_tree_parallel,
    all_formulas = all_formulas,
    final_models = final_models
  ))
)

stopCluster(cl)
run_time/60


# Create counterfactuals --------------------------------------------------

run_time2 <- system.time(counterfactuals_list <- lapply(1:n_id, function(i) {
  predictions <-
    data.table(predicted_energy_kWh = predict(fitted[[i]], prediction_selected[id[i]][[1]]))
  
  context_predictions <- cbind(
    prediction_selected[id[i]][[1]][, .(
    fuel,
    PUPRN,
    Read_date_effective_local,
    energy_kWh,
    month,
    week,
    weekday,
    weekend,
    weekend_or_holiday,
    min_2m_temp_K,
    max_2m_temp_K,
    `2m_temperature_K_day_mean`
  )],
  predictions)
  
  setnames(context_predictions, "energy_kWh", "observed_energy_kWh")
  return(context_predictions)
  }))

run_time2/60

counterfactuals <- rbindlist(counterfactuals_list)

counterfactuals[, obs_min_pred_kWh := observed_energy_kWh - predicted_energy_kWh]

setcolorder(counterfactuals, 
            c("fuel", "PUPRN", "Read_date_effective_local",
              "observed_energy_kWh", "predicted_energy_kWh", "obs_min_pred_kWh"))

counterfactuals[, `:=`(min_2m_temp_C = min_2m_temp_K - 273.15,
                       max_2m_temp_C = max_2m_temp_K - 273.15,
                       mean_2m_temp_C = `2m_temperature_K_day_mean` - 273.15)]


# Save --------------------------------------------------------------------

saveRDS(counterfactuals, 
        paste0(counterfactual_output_folder, fuel, "_counterfactuals.RDS"))
fwrite(counterfactuals,
       paste0(counterfactual_output_folder, fuel, "_counterfactuals.csv"))

