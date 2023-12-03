
# Prepare smart meter data including participant filtering
source("./scripts/create_counterfactuals/data_prep/sm_data_cleaning.R")

# Prepare weather data
source("./scripts/create_counterfactuals/data_prep/climate_data_prep.R")

# Prepare calendar data
source("./scripts/create_counterfactuals/data_prep/calendar_data_prep.R")

# Combine all input data
source("./scripts/create_counterfactuals/data_prep/combine_training_data.R")
source("./scripts/create_counterfactuals/data_prep/combine_prediction_data.R")

# Define formulas
source("./scripts/create_counterfactuals/data_prep/define_formulas.R")

