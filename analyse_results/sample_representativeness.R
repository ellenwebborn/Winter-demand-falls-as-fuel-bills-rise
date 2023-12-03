


# Setup -------------------------------------------------------------------

# packages
library(data.table)

# data loading
energy_results <- readRDS("S:/SERL_Frontier1/EZW/data/analysis_data/intermediary/individual_winter_savings.RDS")
id <- energy_results[, unique(PUPRN)]

orig_survey <- readRDS("./data/survey/serl_survey_data_edition06.RDS")
latest_survey <- readRDS("S:/SERL_Frontier1/EZW/data/survey/COL_survey_processed.rds")
pp_summary <- readRDS("./data/serl_participant_summary_edition06.RDS")

# filtering
orig_survey <- orig_survey[PUPRN %in% id]
latest_survey <- latest_survey[PUPRN %in% id]
pp_summary <- pp_summary[PUPRN %in% id]

n_id <- length(id)

# Join surveys - will pick the latest where available
colnames(orig_survey) <- paste0(colnames(orig_survey), "_orig")
setnames(orig_survey, "PUPRN_orig", "PUPRN")
survey <- merge(latest_survey, orig_survey, all = TRUE, on = "PUPRN")

# Region ------------------------------------------------------------------

regions <- pp_summary[, .(N = .N,
                          tot = nrow(pp_summary)),
                      keyby = Region]
regions[, perc := round(N / tot * 100, 1)]
regions[, variable := "Region"]
setnames(regions, "Region", "Group")


# IMD ---------------------------------------------------------------------

imd <- pp_summary[, .(N = .N,
                          tot = nrow(pp_summary)),
                      keyby = IMD_quintile]
imd[, perc := round(N / tot * 100, 1)]
imd[, variable := "IMD_quintile"]
setnames(imd, "IMD_quintile", "Group")
imd


# EPC rating --------------------------------------------------------------

# Combine A & B and E-G
pp_summary[EPC_rating %in% c("A", "B"), EPC_rating := "A & B"]
pp_summary[EPC_rating %in% c("E", "F", "G"), EPC_rating := "E-G"]
epc <- pp_summary[!is.na(EPC_rating), .(N = .N,
                      tot = pp_summary[!is.na(EPC_rating), .N]),
                  keyby = EPC_rating]
epc[, perc := round(N / tot * 100, 1)]
epc[, variable := "EPC rating"]
setnames(epc, "EPC_rating", "Group")
epc


# Household size ----------------------------------------------------------

survey[, .N, keyby = C1_new_orig]
survey[, .N, keyby = D1]
survey[D1_flag == FALSE, n_occupants := D1]
survey[is.na(n_occupants), n_occupants := C1_new_orig]
survey[, .N, keyby = n_occupants]
survey[n_occupants <= 0, n_occupants := NA]
survey[n_occupants >5, n_occupants := 5] # 5 will henceforth mean 5+, leaving as 5 for simplicity

n_occupants <- survey[!is.na(n_occupants), .(N = .N,
                                             tot = nrow(survey[!is.na(n_occupants)])),
                      keyby = n_occupants]
n_occupants[, perc := round(N / tot * 100, 1)]
n_occupants[, variable := "N occupants"]
setnames(n_occupants, "n_occupants", "Group")
n_occupants




# Tenure ------------------------------------------------------------------

survey[, .N, keyby = B4_orig]

tenure_info <- data.table(B4_orig = 1:5, tenure = c("all owner-occupiers",
                                          "all owner-occupiers",
                                          "private renter or lives rent free",
                                          "social renter",
                                          "private renter or lives rent free"))
survey <- tenure_info[survey, on = "B4_orig"]
survey[, .N, keyby = .(tenure, B4_orig)]
survey[, .N, keyby = .(tenure)]

tenure <- survey[!is.na(tenure), .(N = .N,
                                   tot = nrow(survey[!is.na(tenure)])),
                 keyby = tenure]
tenure[, perc := round(N / tot * 100, 1)]
tenure[, variable := "tenure"]
setnames(tenure, "tenure", "Group")
tenure



# Dwelling ----------------------------------------------------------------

survey[, .N, keyby = .(B1_orig)]

dwelling_info <- data.table(B1_orig = 1:6,
                            dwelling = c("detached", "semi detached", "terraced", 
                                         rep("flat, maisonette or apartment", 3))) # Combining all flats
survey <- dwelling_info[survey, on = "B1_orig"]
survey[, .N, keyby = .(dwelling)]

dwelling <- survey[!is.na(dwelling), .(N = .N,
                                   tot = nrow(survey[!is.na(dwelling)])),
                 keyby = dwelling]
dwelling[, perc := round(N / tot * 100, 1)]
dwelling[, variable := "dwelling"]
setnames(dwelling, "dwelling", "Group")
dwelling




# Household composition ---------------------------------------------------

survey[, .N, keyby = C1_new_orig]
survey[, .N, keyby = .(C2_tot_adult_orig)]
survey[, .N, keyby = .(C2_tot_adult_orig, C2_tot_child_orig)]
survey[, .N, keyby = .(C2_tot_adult_orig, C2_tot_65_plus_orig)]
survey[, .N, keyby = .(C2_tot_child_orig)]

# Get total adults
survey[D1_flag == FALSE & D2_ignored == FALSE & D2_min_total_over16 > 0,
       tot_adult := D2_min_total_over16]
# Update from previous survey if no new data available
survey[is.na(tot_adult) & C2_tot_adult_orig > 0, tot_adult := C2_tot_adult_orig]
survey[, .N, keyby = tot_adult]

# Get total 65+
survey[D1_flag == FALSE & D2_ignored == FALSE & D2_min_total_over16 > 0,
       tot_65_plus := 0]
survey[D2_65_74 == "1 person", tot_65_plus := tot_65_plus + 1]
survey[D2_65_74 == "2 people", tot_65_plus := tot_65_plus + 2]
survey[D2_65_74 == "3 people", tot_65_plus := tot_65_plus + 3]
survey[D2_65_74 == "4 or more people", tot_65_plus := tot_65_plus + 4]

survey[D2_75_84 == "1 person", tot_65_plus := tot_65_plus + 1]
survey[D2_75_84 == "2 people", tot_65_plus := tot_65_plus + 2]
survey[D2_75_84 == "3 people", tot_65_plus := tot_65_plus + 3]
survey[D2_75_84 == "4 or more people", tot_65_plus := tot_65_plus + 4]

survey[D2_85plus == "1 person", tot_65_plus := tot_65_plus + 1]
survey[D2_85plus == "2 people", tot_65_plus := tot_65_plus + 2]
survey[D2_85plus == "3 people", tot_65_plus := tot_65_plus + 3]
survey[D2_85plus == "4 or more people", tot_65_plus := tot_65_plus + 4]

survey[, .N, keyby = tot_65_plus]
survey[, .N, keyby = C2_tot_65_plus_orig]

# Update from previous survey if no new data available
survey[is.na(tot_65_plus) & C2_tot_65_plus_orig != -2, 
       tot_65_plus := C2_tot_65_plus_orig]

# Get total children
survey[D1_flag == FALSE & D2_ignored == FALSE & D2_min_total_over16 > 0,
       tot_child := 0]
survey[D2_0_5 == "1 person", tot_child := tot_child + 1]
survey[D2_0_5 == "2 people", tot_child := tot_child + 2]
survey[D2_0_5 == "3 people", tot_child := tot_child + 3]
survey[D2_0_5 == "4 or more people", tot_child := tot_child + 4]

survey[D2_6_15 == "1 person", tot_child := tot_child + 1]
survey[D2_6_15 == "2 people", tot_child := tot_child + 2]
survey[D2_6_15 == "3 people", tot_child := tot_child + 3]
survey[D2_6_15 == "4 or more people", tot_child := tot_child + 4]

survey[, .N, keyby = tot_child]
survey[, .N, keyby = C2_tot_child_orig]

# Update from previous survey if no new data available
survey[is.na(tot_child) & C2_tot_child_orig != -2, 
       tot_child := C2_tot_child_orig]


survey[tot_adult == 1 & tot_65_plus == 1 & tot_child == 0, composition := "1-person aged 65+"] 
survey[tot_adult == 1 & tot_65_plus == 0  & tot_child == 0, composition := "1-person aged 64-"]
survey[tot_adult <= 1 & tot_child > 0, composition := "1 or 0 adults & 1+ children"]
survey[tot_adult == 2 & tot_65_plus == 1 & tot_child == 0, composition := "1 65+, 1 65-, 0 children"]
survey[tot_adult == 2 & tot_65_plus == 2 & tot_child == 0, composition := "2 65+, 0 children"]
survey[tot_adult == 2 & tot_65_plus == 0 & tot_child == 0, composition := "2 65-, 0 children"]
survey[tot_adult == 2 & tot_child %in% c(1,2), composition := "2 adults, 1 or 2 children"]
survey[tot_adult == 2 & tot_child >= 3, composition := "2 adults, 3+ children"]
survey[tot_adult >= 3 & tot_child >= 1, composition := "3+ adults, 1+ children"]
survey[tot_adult >= 3 & tot_child == 0, composition := "3+ adults, 0 children"]

survey[, .N, keyby = composition]


household_composition <- survey[!is.na(composition), .(N = .N,
                                       tot = nrow(survey[!is.na(composition)])),
                   keyby = composition]
household_composition[, perc := round(N / tot * 100, 1)]
household_composition[, variable := "household composition"]
setnames(household_composition, "composition", "Group")
household_composition




# export ------------------------------------------------------------------

# Combine
sample_representativeness <- rbind(regions, imd, epc,
                                   n_occupants, tenure, dwelling,
                                   household_composition)
sample_representativeness[, min(N)]
setcolorder(sample_representativeness,
            c("variable", "Group", "tot", "N", "perc"))

# Save
fwrite(sample_representativeness, 
       "./data/outputs/sample_representativeness.csv")

