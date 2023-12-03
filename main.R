

# All scripts for the Cost-of-living crisis paper
# All can be run independently, some should be run for gas and electricity separately
# All inputs controlled by setup.R which is sourced in each file below



# Data prep ---------------------------------------------------------------

# Prepare all input data
source("./scripts/create_counterfactuals/00_prepare_input_data.R")


# Create counterfactuals --------------------------------------------------

# Train a model using definitions in setup_colc.R
source("./scripts/create_counterfactuals/01_train_model.R")

# Compare the latest model to all previous, select households for retraining
source("./scripts/create_counterfactuals/02_compare_trained_models.R")

# Get final model train and test errors 
source("./scripts/create_counterfactuals/03_get_train_and_test_errors.R")

# retrain selected model on full training set
source("./scripts/create_counterfactuals/04_retrain_on_full_train_test_set.R")

# add contextual data for analysis and combine gas and elec
source("./scripts/create_counterfactuals/05_format_counterfactuals_for_analysis.R")

# Filter training data on final participants included
source("./scripts/create_counterfactuals/06_get_prev_winter_observations.R")

# Add price cap data
source("./scripts/create_counterfactuals/07_add_price_data.R")



# Results analysis --------------------------------------------------------

source("./scripts/analyse_results/rq1_overall_savings.R")

source("./scripts/analyse_results/rq2_temperature_effects.R")

source("./scripts/analyse_results/rq4c_energy_saving_behaviours.R")

source("./scripts/analyse_results/rq5_energy_costs.R")

source("./scripts/analyse_results/rq6b_biggest_savers.R")

source("./scripts/analyse_results/rq7_price_elasticity.R")

source("./scripts/analyse_results/sample_representativeness.R")

