


# setup -------------------------------------------------------------------

source("./scripts/setup_colc.R")
source("./scripts/functions/train_colc_models.R")
source("./scripts/functions/unregister_dopar.R")
source("./scripts/functions/xgb_tree.R")


# Load data ---------------------------------------------------------------

if(fuel == "elec") {
  id <- readRDS(elec_id_file)
  training_data <- readRDS(elec_training_data_file)
} else {
  id <- readRDS(gas_id_file)
  training_data <- readRDS(gas_training_data_file)
}

if(file.exists(id_to_retrain_file)) {
  id_to_retrain <- readRDS(id_to_retrain_file)
  names(training_data) <- id
  training_data <- training_data[id_to_retrain]
} else {id_to_retrain <- id}

n_id <- length(id_to_retrain)
n_id

# Import formulas
formula_files <- list.files("./scripts/regression_formulas/")
all_formulas <- lapply(formula_files, function(f) {
  readRDS(paste0("./scripts/regression_formulas/", f))
})
names(all_formulas) <- str_remove(formula_files, ".RDS")


if(use_best_formula == TRUE) {
  formulas <- readRDS(retrain_formula_file)
  formula_name <- formulas[PUPRN %in% id_to_retrain]$formula
}



# train model -------------------------------------------------------------

# Set up parallel environment
unregister_dopar()
no_cores <- detectCores(logical = TRUE)
cl <- makeCluster(no_cores - 1)
registerDoParallel(cl)
clusterEvalQ(cl, 
             {library(data.table); 
               library(caret); 
               library(xgboost); 
               library(plyr)}
             )

# Export data to cluster
clusterExport(cl, list('kfolds',
                       'algorithm',
                       'hyper_params_to_test',
                       'xgb_tree'))

# train model
run_time <- system.time(model_training_outputs <- c(
  parLapply(
    cl,
    1:n_id,
    fun = train_colc_models,
    training_data = training_data,
    prediction_data = NA,
    my_formula = formula_name,
    all_formulas = all_formulas,
    get_training_error = TRUE,
    use_holdout = TRUE
  )
))

stopCluster(cl)
run_time / 60

if(length(formula_name) > 1) {formula_name <- "optimal_f"}

# save model outputs for future comparison
model_training_outputs2 <- append(model_training_outputs,
                                  list(hyperparameters = hyper_params_to_test,
                                       formula = formula_name,
                                       algorithm = algorithm,
                                       runtime_mins = run_time[3]/60))


saveRDS(model_training_outputs2, 
        paste0(model_training_folder, fuel, "_", algorithm, "_", 
               formula_name, "_", output_file_suffix))


