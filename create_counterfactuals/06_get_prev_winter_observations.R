

# setup -------------------------------------------------------------------

source("./scripts/setup_colc.R")

elec_training <- rbindlist(readRDS(paste0("S:/SERL_Frontier1/EZW/data/processed/", data_creation_date, 
                                          "/elec_training_data.RDS")))
gas_training <- rbindlist(readRDS(paste0("S:/SERL_Frontier1/EZW/data/processed/", data_creation_date, 
                                         "/gas_training_data.RDS")))

elec_id <- readRDS(paste0(final_models_folder, "elec_puprn_sufficiently_accurate.RDS"))
gas_id <- readRDS(paste0(final_models_folder, "gas_puprn_sufficiently_accurate.RDS"))



# Filter ------------------------------------------------------------------


elec_training <- elec_training[PUPRN %in% elec_id, 
                               .(PUPRN, fuel, Read_date_effective_local, month, energy_kWh)]
gas_training <- gas_training[PUPRN %in% gas_id, 
                               .(PUPRN, fuel, Read_date_effective_local, month, energy_kWh)]
all_training <- rbind(elec_training, gas_training)


# Save --------------------------------------------------------------------

saveRDS(all_training,
        "S:/SERL_Frontier1/EZW/data/model_training_outputs/prev_winter_observations.RDS")
