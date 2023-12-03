

# setup -------------------------------------------------------------------

source("./scripts/setup_colc.R")

# Load data
cf_long <- readRDS("./data/model_training_outputs/all_counterfactuals/all_cf_long.RDS")
winter_costs <- readRDS("./data/winter_costs.RDS")
partic_info <- readRDS("./data/serl_participant_summary_edition06.RDS")

all_training <- readRDS("S:/SERL_Frontier1/EZW/data/model_training_outputs/prev_winter_observations.RDS")


# Handle LSOAs belonging to multiple regions ------------------------------

winter_costs <-
  winter_costs[, .(
    elec_uc = mean(ElectricityUC),
    elec_sc = mean(ElecSC),
    gas_uc = mean(GasUC),
    gas_sc = mean(GasSC)
  ),
  keyby = .(LSOA, period)]


# Add LSOA to counterfactuals
cf_long <- partic_info[, .(PUPRN, LSOA)][cf_long, on = "PUPRN"]

winter_costs[, .N, keyby = period]
# Jan_Mar_23
# Oct21_Mar22
# Oct_Dec_22

# Add periods to counterfactuals
cf_long[Read_date_effective_local < "2023-01-01", period := "Oct_Dec_22"]
cf_long[Read_date_effective_local >= "2023-01-01", period := "Jan_Mar_23"]

cf_long[, .N, keyby = period] # All covered (only prediction)

# Elec
elec_cf <- cf_long[fuel == "elec"]
elec_cf <- winter_costs[, .(LSOA, period, elec_uc, elec_sc)][elec_cf, 
                                                             on = c("LSOA", "period")]
setnames(elec_cf, 
         c("elec_sc", "elec_uc"), 
         c("standing_charge", "unit_cost"))

# Gas
gas_cf <- cf_long[fuel == "gas"]
gas_cf <- winter_costs[, .(LSOA, period, gas_uc, gas_sc)][gas_cf, 
                                                          on = c("LSOA", "period")]
setnames(gas_cf, 
         c("gas_sc", "gas_uc"), 
         c("standing_charge", "unit_cost"))

# Both
all_cf <- rbind(elec_cf, gas_cf)

all_cf[variable %in% c("observed_energy_kWh",
                        "predicted_energy_kWh"), 
        daily_cost_pounds := (standing_charge + unit_cost * value) * 1.05 / 100] # 1.05 is VAT

# Difference doesn't involve standing charge
all_cf[variable == "savings_kWh", daily_cost_pounds := unit_cost * value * 1.05 / 100] # 1.05 is VAT

setcolorder(all_cf, c("PUPRN", "fuel"))



# Previous winter data ----------------------------------------------------

elec_training <- all_training[fuel == "elec", .(PUPRN, fuel, Read_date_effective_local, energy_kWh)]
gas_training <- all_training[fuel == "gas", .(PUPRN, fuel, Read_date_effective_local, energy_kWh)]

elec_training[, period := "Oct21_Mar22"]
gas_training[, period := "Oct21_Mar22"]

elec_training <- partic_info[, .(PUPRN, LSOA)][elec_training, on = "PUPRN"]
gas_training <- partic_info[, .(PUPRN, LSOA)][gas_training, on = "PUPRN"]

elec_training <- winter_costs[, .(LSOA, period, elec_uc, elec_sc)][elec_training, 
                                                             on = c("LSOA", "period")]
setnames(elec_training, 
         c("elec_sc", "elec_uc"), 
         c("standing_charge", "unit_cost"))

gas_training <- winter_costs[, .(LSOA, period, gas_uc, gas_sc)][gas_training, 
                                                                   on = c("LSOA", "period")]
setnames(gas_training, 
         c("gas_sc", "gas_uc"), 
         c("standing_charge", "unit_cost"))

all_training <- rbind(elec_training, gas_training)

all_training[, daily_cost_pounds := (standing_charge + unit_cost * energy_kWh) * 1.05 / 100]

all_training[, month := month(Read_date_effective_local)]



# Save --------------------------------------------------------------------

saveRDS(all_cf,
        "./data/model_training_outputs/all_counterfactuals/cf_with_costs.RDS")

saveRDS(all_training, 
        paste0("S:/SERL_Frontier1/EZW/data/processed/", data_creation_date, "/all_training_with_costs.RDS"))
        
        
        
        
        
        