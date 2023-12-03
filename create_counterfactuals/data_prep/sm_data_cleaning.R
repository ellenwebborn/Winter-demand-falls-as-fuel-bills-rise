# Clean smart meter training data


# Setup and loading -------------------------------------------------------

source("./scripts/setup_colc.R")

all_daily <- readRDS("./data/daily_sm/serl_smart_meter_daily_edition06.RDS")
daily_training <- all_daily[Read_date_effective_local >= "2021-10-01" & 
                               Read_date_effective_local <= "2022-03-31"]
daily_prediction <- all_daily[Read_date_effective_local >= "2022-10-01" & 
                               Read_date_effective_local <= "2023-03-31"]

survey_2023 <- readRDS("./data/survey/COL_survey_processed.rds")



# Electricity training data prep ---------------------------------------------------

elec_daily_training <- daily_training[Valid_hh_sum_or_daily_elec == TRUE, 
                                      .(PUPRN, Read_date_effective_local,
                                        Unit_correct_elec_act_imp_d_Wh, 
                                        Elec_act_imp_hh_sum_Wh)]

# keep the half-hourly sums unless invalid, in which case use daily value
elec_daily_training[, energy_kWh := Elec_act_imp_hh_sum_Wh / 1000]
elec_daily_training[is.na(Elec_act_imp_hh_sum_Wh), energy_kWh := Unit_correct_elec_act_imp_d_Wh / 1000]
elec_daily_training <- elec_daily_training[, .(PUPRN, Read_date_effective_local, energy_kWh)]

length(unique(elec_daily_training$PUPRN)) 

# Remove anyone with all zeros in their data
max_elec_vals <- elec_daily_training[, max(energy_kWh), keyby = PUPRN]
elec_zero_removals <- max_elec_vals[V1 <= 0, PUPRN]
elec_daily_training <- elec_daily_training[!PUPRN %in% elec_zero_removals]

length(unique(elec_daily_training$PUPRN)) 


# Gas training data prep -----------------------------------------------------------

gas_daily_training <- daily_training[Valid_hh_sum_or_daily_gas == TRUE, 
                                      .(PUPRN, Read_date_effective_local,
                                        Gas_d_m3, 
                                        Gas_hh_sum_m3)]

# keep the half-hourly sums unless invalid, in which case use daily value
gas_daily_training[, energy_kWh := Gas_hh_sum_m3 * 1.02264 * 39.5 / 3.6]
gas_daily_training[is.na(Gas_hh_sum_m3), energy_kWh := Gas_d_m3 * 1.02264 * 39.5 / 3.6]
gas_daily_training <- gas_daily_training[, .(PUPRN, Read_date_effective_local, energy_kWh)]

length(unique(gas_daily_training$PUPRN)) # 9009

# Remove anyone with all zeros in their data
max_gas_vals <- gas_daily_training[, max(energy_kWh), keyby = PUPRN]
gas_zero_removals <- max_gas_vals[V1 <= 0, PUPRN]
gas_daily_training <- gas_daily_training[!PUPRN %in% gas_zero_removals]

length(unique(gas_daily_training$PUPRN)) # 8972


# Minimum data availability requirements ----------------------------------

# Elec
n_elec_per_month <- elec_daily_training[, .N, keyby = .(PUPRN, month(Read_date_effective_local))]
n_elec_per_month_over_25 <- n_elec_per_month[N >= 25]
n_elec_all_six_months_valid <- n_elec_per_month_over_25[, .N, keyby = PUPRN]
n_elec_all_six_months_valid[, .N, keyby = N]
n_elec_all_six_months_valid <- n_elec_all_six_months_valid[N == 6]
nrow(n_elec_all_six_months_valid) 
n_elec_all_six_months_valid[, fuel := "elec"]

# Gas
n_gas_per_month <- gas_daily_training[, .N, keyby = .(PUPRN, month(Read_date_effective_local))]
n_gas_per_month_over_25 <- n_gas_per_month[N >= 25]
n_gas_all_six_months_valid <- n_gas_per_month_over_25[, .N, keyby = PUPRN]
n_gas_all_six_months_valid[, .N, keyby = N]
n_gas_all_six_months_valid <- n_gas_all_six_months_valid[N == 6]
nrow(n_gas_all_six_months_valid) 
n_gas_all_six_months_valid[, fuel := "gas"]



# Filter training data on selected participants --------------------------------

elec_daily_training_selected <- n_elec_all_six_months_valid[elec_daily_training, on = "PUPRN"]
elec_daily_training_selected[, .N, keyby = fuel]
elec_daily_training_selected <- elec_daily_training_selected[fuel == "elec", 
                                                             .(PUPRN, 
                                                               Read_date_effective_local, 
                                                               energy_kWh, fuel)]
length(unique(elec_daily_training_selected$PUPRN)) 

gas_daily_training_selected <- n_gas_all_six_months_valid[gas_daily_training, on = "PUPRN"]
gas_daily_training_selected[, .N, keyby = fuel]
gas_daily_training_selected <- gas_daily_training_selected[fuel == "gas", 
                                                           .(PUPRN, 
                                                             Read_date_effective_local, 
                                                             energy_kWh, fuel)]
length(unique(gas_daily_training_selected$PUPRN)) 



# Prediction electricity data prep ---------------------------------------------------

elec_pred_daily <- daily_prediction[Valid_hh_sum_or_daily_elec == TRUE & 
                                      PUPRN %in% unique(elec_daily_training_selected$PUPRN), 
                    .(PUPRN, Read_date_effective_local,
                      Unit_correct_elec_act_imp_d_Wh, 
                      Elec_act_imp_hh_sum_Wh)]

# keep the half-hourly sums unless invalid, in which case use daily value
elec_pred_daily[, energy_kWh := Elec_act_imp_hh_sum_Wh / 1000]
elec_pred_daily[is.na(Elec_act_imp_hh_sum_Wh), energy_kWh := Unit_correct_elec_act_imp_d_Wh / 1000]
elec_pred_daily <- elec_pred_daily[, .(PUPRN, Read_date_effective_local, energy_kWh)]

length(unique(elec_pred_daily$PUPRN)) 

n_elec_valid <- elec_pred_daily[, .N, keyby = .(PUPRN)] # Expect 182 if all there 
# Change requirement to 90% valid ie 164 or more days
n_elec_all_valid <- n_elec_valid[N >= 164]
nrow(n_elec_all_valid) 

elec_pred_daily_selected <- n_elec_all_valid[elec_pred_daily, on = "PUPRN"]
elec_pred_daily_selected <- elec_pred_daily_selected[!is.na(N)]
elec_pred_daily_selected[, N := NULL]
elec_pred_daily_selected[, fuel := "elec"]
length(unique(elec_pred_daily_selected$PUPRN)) 



# Prediction gas data prep ------------------------------------------------

gas_pred_daily <- daily_prediction[Valid_hh_sum_or_daily_gas == TRUE & 
                                     PUPRN %in% unique(gas_daily_training_selected$PUPRN), 
                   .(PUPRN, Read_date_effective_local,
                     Gas_d_m3, 
                     Gas_hh_sum_m3)]

# keep the half-hourly sums unless invalid, in which case use daily value
gas_pred_daily[, energy_kWh := Gas_hh_sum_m3 * 1.02264 * 39.5 / 3.6]
gas_pred_daily[is.na(Gas_hh_sum_m3), energy_kWh := Gas_d_m3 * 1.02264 * 39.5 / 3.6]
gas_pred_daily <- gas_pred_daily[, .(PUPRN, Read_date_effective_local, energy_kWh)]

length(unique(gas_pred_daily$PUPRN))

n_gas_valid <- gas_pred_daily[, .N, keyby = .(PUPRN)] # Expect 182 if all there 
# Change requirement to 90% valid ie 164 or more days
n_gas_all_valid <- n_gas_valid[N >= 164]
nrow(n_gas_all_valid) 

gas_pred_daily_selected <- n_gas_all_valid[gas_pred_daily, on = "PUPRN"]
gas_pred_daily_selected <- gas_pred_daily_selected[!is.na(N)]
gas_pred_daily_selected[, N := NULL]
gas_pred_daily_selected[, fuel := "gas"]

length(unique(gas_pred_daily_selected$PUPRN)) 



# Filter training data following prediction filtering ---------------------

elec_id <- unique(elec_pred_daily_selected$PUPRN)
elec_daily_training_selected <- elec_daily_training_selected[PUPRN %in% elec_id]
length(unique(elec_daily_training_selected$PUPRN)) 

gas_id <- unique(gas_pred_daily_selected$PUPRN)
gas_daily_training_selected <- gas_daily_training_selected[PUPRN %in% gas_id]
length(unique(gas_daily_training_selected$PUPRN)) 

length(intersect(elec_id, gas_id)) 



# Filter participants we wish to exclude ----------------------------------

survey_2023[, recent_EV_exclude := (D6 %in% c("1", "2", "3 or more", "Don't know"))]
survey_2023[, recent_HP_exclude := (B2_5__add_rep == "Has been added or replaced in the last 12 months")]

# Filter elec
length(elec_id) 
elec_daily_training_selected2 <- survey_2023[, .(PUPRN, recent_EV_exclude, 
                                                 recent_HP_exclude)][elec_daily_training_selected,
                                                                     on = "PUPRN"]
elec_daily_training_selected2 <- elec_daily_training_selected2[(recent_EV_exclude == FALSE | 
                                                                  is.na(recent_EV_exclude)) & 
                                                                 (recent_HP_exclude == FALSE | 
                                                                    is.na(recent_HP_exclude))]
length(unique(elec_daily_training_selected2$PUPRN)) 
length(elec_id) - length(unique(elec_daily_training_selected2$PUPRN)) 


#Remove anyone with all zeros in the training or prediction set
#Training
zeros_exclusions <- elec_daily_training_selected2[energy_kWh == 0, .N, keyby = PUPRN]
zeros_exclusions[, exclude_on_zeros := N > 182/2]

elec_daily_training_selected2 <- zeros_exclusions[, .(PUPRN, exclude_on_zeros)][elec_daily_training_selected2,
                                                  on = "PUPRN"]
elec_daily_training_selected2[exclude_on_zeros == TRUE, length(unique(PUPRN))] 

#Prediction filter
zeros_exclusions <- elec_pred_daily_selected[energy_kWh == 0, .N, keyby = PUPRN]
zeros_exclusions[, exclude_on_pred_zeros := N > 182/2]

elec_daily_training_selected2 <- zeros_exclusions[, .(PUPRN, exclude_on_pred_zeros)][elec_daily_training_selected2,
                                                                                on = "PUPRN"]
elec_daily_training_selected2[exclude_on_pred_zeros == TRUE | exclude_on_zeros == TRUE,
                              length(unique(PUPRN))] 

elec_daily_training_selected2 <- elec_daily_training_selected2[(exclude_on_zeros == FALSE | 
                                                                  is.na(exclude_on_zeros)) & 
                                                                 (exclude_on_pred_zeros == FALSE | 
                                                                    is.na(exclude_on_pred_zeros))]


# update prediction data and id
elec_id <- unique(elec_daily_training_selected2$PUPRN)
elec_pred_daily_selected2 <- elec_pred_daily_selected[PUPRN %in% elec_id]

elec_daily_training_selected2[, `:=`(recent_EV_exclude = NULL,
                                     recent_HP_exclude = NULL,
                                     exclude_on_zeros = NULL,
                                     exclude_on_pred_zeros = NULL)]


# Filter gas
length(gas_id) 
gas_daily_training_selected2 <- survey_2023[, .(PUPRN, recent_HP_exclude)][gas_daily_training_selected,
                                                                     on = "PUPRN"]
gas_daily_training_selected2 <- gas_daily_training_selected2[recent_HP_exclude == FALSE | 
                                                                    is.na(recent_HP_exclude)]
length(unique(gas_daily_training_selected2$PUPRN)) 
length(gas_id) - length(unique(gas_daily_training_selected2$PUPRN)) 


#Remove anyone with all zeros in the training or prediction set
#Training
zeros_exclusions <- gas_daily_training_selected2[energy_kWh == 0, .N, keyby = PUPRN]
zeros_exclusions[, exclude_on_zeros := N > 182 * 0.9]

gas_daily_training_selected2 <- zeros_exclusions[, .(PUPRN, exclude_on_zeros)][gas_daily_training_selected2,
                                                                                on = "PUPRN"]
gas_daily_training_selected2[exclude_on_zeros == TRUE, length(unique(PUPRN))] 

#Prediction filter
zeros_exclusions <- gas_pred_daily_selected[energy_kWh == 0, .N, keyby = PUPRN]
zeros_exclusions[, exclude_on_pred_zeros := N > 182 * 0.9]

gas_daily_training_selected2 <- zeros_exclusions[, .(PUPRN, exclude_on_pred_zeros)][gas_daily_training_selected2,
                                                                                     on = "PUPRN"]
gas_daily_training_selected2[exclude_on_pred_zeros == TRUE | exclude_on_zeros == TRUE,
                              length(unique(PUPRN))] 

gas_daily_training_selected2 <- gas_daily_training_selected2[(exclude_on_zeros == FALSE | 
                                                                  is.na(exclude_on_zeros)) & 
                                                                 (exclude_on_pred_zeros == FALSE | 
                                                                    is.na(exclude_on_pred_zeros))]



# update prediction data and id
gas_id <- unique(gas_daily_training_selected2$PUPRN)
length(gas_id) 
gas_pred_daily_selected2 <- gas_pred_daily_selected[PUPRN %in% gas_id]
length(unique(gas_pred_daily_selected2$PUPRN)) 
gas_daily_training_selected2[, `:=`(recent_HP_exclude = NULL,
                                    exclude_on_zeros = NULL,
                                    exclude_on_pred_zeros = NULL)]


# Save --------------------------------------------------------------------


all_training_energy_data <- rbind(elec_daily_training_selected2,
                                  gas_daily_training_selected2)

saveRDS(all_training_energy_data, 
        file = paste0(input_data_folder, "all_training_energy_data"))

all_prediction_energy_data <- rbind(elec_pred_daily_selected2,
                                    gas_pred_daily_selected2)

saveRDS(all_prediction_energy_data, 
        file = paste0(input_data_folder, "all_prediction_energy_data"))
