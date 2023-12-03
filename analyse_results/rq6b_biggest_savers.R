# RQ6 correlation between household characteristics and energy saving 
#  (RQ3 and RQ6 in the paper)

# setup -------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(lubridate)
library(epitools)
library(caret)
library(broom)

source("./scripts/functions/get_odds_ratio_data.R")
source("./scripts/functions/get_big_saver_characteristics.R")
source("./scripts/functions/tidy.train.R")
source("./scripts/functions/calc_risk_ratio_big_saver_given_action.R")

colc_survey <- readRDS("./data/survey/COL_survey_processed.rds")
energy_results <- readRDS("./data/analysis_data/intermediary/individual_winter_savings.RDS")
pp_summary <- readRDS("./data/serl_participant_summary_edition06.RDS")
orig_survey <- readRDS("./data/survey/serl_survey_data_edition06.RDS")
energy_with_totals <- readRDS("./data/model_training_outputs/all_counterfactuals/winter_mean_total_fuel.RDS")

# Combine
all_data <- energy_results[variable == "observed_energy_kWh" & fuel == "elec", 
                           .(PUPRN, saving_perc_quintile)]
all_data <- colc_survey[all_data, on = "PUPRN"]
all_data <- pp_summary[, .(PUPRN, Region, IMD_quintile, EPC_rating)][all_data, on = "PUPRN"]
all_data[, 1:16]
all_data[, group := ifelse(saving_perc_quintile == 5, "big_saver", "other")]
all_data[, group := factor(group, levels = c("other", "big_saver"))]

colnames(orig_survey) <- paste0(colnames(orig_survey), "_orig")
setnames(orig_survey, "PUPRN_orig", "PUPRN")

all_data <- orig_survey[all_data, on = "PUPRN"]



# Household characteristics -----------------------------------------------

pp_summary[, 1:10]


#### IMD ####
imd <- get_big_saver_characteristics(all_data, "IMD_quintile")

imd_or <- get_odds_ratio_data(all_data, variable = "IMD_quintile")
imd_or

ggplot(imd, aes(x = subgroup, y = perc, fill = group)) + 
  geom_bar(stat = "identity", position = "dodge")


#### EPC ####
all_data[EPC_rating %in% c("A", "B"), EPC_rating := "A & B"]
all_data[EPC_rating %in% c("E", "F", "G"), EPC_rating := "E-G"]

epc <- get_big_saver_characteristics(all_data[!is.na(EPC_rating)], "EPC_rating")

all_data[, .N, keyby = EPC_rating]
all_data[, EPC_rating := factor(EPC_rating, levels = c("C", "D", "E-G", "A & B"))]

epc_or <- get_odds_ratio_data(all_data, variable = "EPC_rating")
epc_or

ggplot(epc, aes(x = subgroup, y = perc, fill = group)) + 
  geom_bar(stat = "identity", position = "dodge")


#### Dwelling type ####

all_data[, .N, keyby = B1_orig]

dwellings <- data.table(B1_orig = 1:6,
                        dwelling = c("detached", "semi-detached", "terraced",
                                     rep("flat, maisonette or apartment", 3)))
all_data <- dwellings[all_data, on = "B1_orig"]
all_data[, .N, keyby = dwelling]
all_data[, dwelling := factor(dwelling, levels = c("semi-detached", "detached", 
                                                   "terraced", "flat, maisonette or apartment"))]


dwelling_type <- get_big_saver_characteristics(all_data[!is.na(dwelling)], 
                                               "dwelling", 
                                               "Dwelling type")

ggplot(dwelling_type, aes(x = subgroup, y = perc, fill = group)) + 
  geom_bar(stat = "identity", position = "dodge")


#### Tenure ####
all_data[, .N, keyby = B4_orig]

tenures <- data.table(B4_orig = 1:5, 
                      tenure = c(rep("Own or part-own", 2),
                                 "Rent privately", 
                                 "Social renter",
                                 "Live rent free"))
all_data <- tenures[all_data, on = "B4_orig"]
all_data[, .N, keyby = tenure]
all_data[, tenure := factor(tenure, levels = c("Own or part-own", 
                                               "Rent privately", 
                                               "Social renter",
                                               "Live rent free"))]


tenure <- get_big_saver_characteristics(all_data[!is.na(tenure)], 
                                               "tenure", 
                                               "Tenure")

#### N bedrooms ####
all_data[, .N, keyby = .(B6_orig, B6_err_orig)]
all_data[B6_err_orig == FALSE & B6_orig > 0, bedrooms := B6_orig]
all_data[bedrooms > 5, bedrooms := 5]
all_data[, .N, keyby = bedrooms]
all_data[, bedrooms := factor(bedrooms,
                               levels = c(3, 1, 2, 4, 5))]
bedrooms <- get_big_saver_characteristics(all_data[!is.na(bedrooms)], 
                                        "bedrooms", 
                                        "Number of bedrooms")
all_data[B6_err_orig == FALSE & B6_orig > 0, bedrooms_cts := B6_orig]
all_data[, .N, keyby = bedrooms_cts]



#### House age ####
all_data[, .N, keyby = B9_orig]
all_data[B9_orig > 0, house_age := B9_orig]
all_data[, house_age := factor(house_age, levels = c(4, 1:3, 5:7))]
all_data[, .N, keyby = house_age]

house_age <- get_big_saver_characteristics(all_data[!is.na(house_age)], 
                                          "house_age", 
                                          "Age of dwelling")

ggplot(house_age, aes(x = subgroup, y = perc, fill = group)) + 
  geom_bar(stat = "identity", position = "dodge")


#### Number of occupants ####
all_data[, .N, keyby = .(D1_flag, D1)]

all_data[D1_flag == FALSE & D1 > 0, n_occupants := D1]
all_data[, .N, keyby = n_occupants]

# Combine 5+ occupants
all_data[n_occupants > 5, n_occupants := 5]
all_data[, n_occupants := as.character(n_occupants)]
all_data[n_occupants == "5", n_occupants := "5+"]
all_data[, .N, keyby = n_occupants]
all_data[, n_occupants := factor(n_occupants, levels = c("2", "1", "3", "4", "5+"))]

n_occupants <- get_big_saver_characteristics(all_data[!is.na(n_occupants)], 
                                             "n_occupants", 
                                             "Number of occupants")
n_occupants
ggplot(n_occupants, aes(x = subgroup, y = perc, fill = group)) + 
  geom_bar(stat = "identity", position = "dodge")

all_data[D1_flag == FALSE & D1 > 0, occupants_cts := D1]
all_data[, .N, keyby = occupants_cts]



#### Financial Wellbeing ####
all_data[, .N, keyby = E1]

all_data[E1 %in% c("Living comfortably",
                   "Doing alright"), combo_financial_wellbeing := "High"]
all_data[E1 %in% c("Finding it very difficult",
                   "Finding it quite difficult",
                   "Just about getting by"), combo_financial_wellbeing := "Low"]
all_data[, .N, keyby = combo_financial_wellbeing]

financial_wellbeing <- get_big_saver_characteristics(all_data[!is.na(combo_financial_wellbeing)], 
                                                     "combo_financial_wellbeing",
                                                     "Financial wellbeing")

all_data[, combo_financial_wellbeing := factor(combo_financial_wellbeing, 
                                               levels = c("High", "Low"))]
financial_wellbeing_or <- get_odds_ratio_data(all_data[!is.na(combo_financial_wellbeing)],
                                              variable = "combo_financial_wellbeing",
                                              name = "Financial wellbeing")
financial_wellbeing_or


#### Ease of meeting heating costs ####
all_data[C5 %in% c("Very easy", "Fairly easy"), 
         combo_C5 := "Very or fairly easy"]
all_data[C5 == "Neither easy nor difficult", 
         combo_C5 := "Neither easy nor difficult"]
all_data[C5 %in% c("Very difficult", "Fairly difficult"), 
         combo_C5 := "Very or fairly difficult"]

all_data[, combo_C5 := factor(combo_C5, levels = c("Very or fairly easy",
                                                   "Neither easy nor difficult",
                                                   "Very or fairly difficult"))]

all_data[, .N, keyby = combo_C5]
ease_meeting_heating_costs <- get_big_saver_characteristics(all_data[!is.na(combo_C5)], 
                                                            "combo_C5",
                                                            "Ease of meeting heating costs")
ease_meeting_heating_costs
ease_meeting_heating_costs_or <- get_odds_ratio_data(all_data[!is.na(combo_C5)],
                                                     variable = "combo_C5",
                                                     name = "Ease of meeting heating costs")
ease_meeting_heating_costs_or


#### Keeping comfortably warm ####
all_data[, .N, keyby = C3]
all_data[!C3 %in% c("Don't know", "No response"), 
         C3_combo := ifelse(C3 == "Yes", 
                            "Can keep comfortably warm", 
                            "Can't keep comfortably warm")]
all_data[, C3_combo := factor(C3_combo, levels = c("Can keep comfortably warm",
                                                   "Can't keep comfortably warm"))]

all_data[, .N, keyby = C3_combo]
keep_comfortably_warm <- get_big_saver_characteristics(all_data[!is.na(C3_combo)], 
                                                       "C3_combo",
                                                       "Keeping comfortably warm")
keep_comfortably_warm
keep_comfortably_warm_or <- get_odds_ratio_data(all_data[!is.na(C3_combo)],
                                                variable = "C3_combo",
                                                name = "Keeping comfortably warm")
keep_comfortably_warm_or


#### Children under 6 ####
all_data[, .N, keyby = D2_0_5]

all_data[D2_0_5 %in% c("1 person", "2 people", 
                       "3 people", "4 or more people"), any_under_6 := TRUE]
all_data[D2_0_5 == "0 people", any_under_6 := FALSE]

all_data[, .N, keyby = any_under_6]
any_under_6 <- get_big_saver_characteristics(all_data[!is.na(any_under_6)], 
                                             "any_under_6",
                                             "Any children under 6yrs")
any_under_6
any_under_6_or <- get_odds_ratio_data(all_data[!is.na(any_under_6)], 
                                      "any_under_6",
                                      "Any children under 6yrs")

#### Any children ####
all_data[, .N, keyby = D2_6_15]

all_data[D2_6_15 %in% c("1 person", "2 people", 
                        "3 people", "4 or more people") |
           any_under_6 == TRUE, any_children := TRUE]
all_data[D2_6_15 == "0 people" & any_under_6 == FALSE, any_children := FALSE]
all_data[, .N, keyby = any_children]

any_children <- get_big_saver_characteristics(all_data[!is.na(any_children)], 
                                              "any_children",
                                              "Any children under 16yrs")
any_children
any_children_or <- get_odds_ratio_data(all_data[!is.na(any_children)], 
                                       "any_children",
                                       "Any children under 16yrs")
any_children_or


#### Adults over 85 ####
all_data[, .N, keyby = D2_85plus]
all_data[D2_85plus %in% c("1 person", "2 people", 
                       "3 people", "4 or more people"), any_85_plus := TRUE]
all_data[D2_85plus == "0 people", any_85_plus := FALSE]

all_data[, .N, keyby = any_85_plus]
any_85_plus <- get_big_saver_characteristics(all_data[!is.na(any_85_plus)], 
                                             "any_85_plus",
                                             "Any aged 85+yrs")
any_85_plus
any_85_plus_or <- get_odds_ratio_data(all_data[!is.na(any_85_plus)], 
                                      "any_85_plus",
                                      "Any aged 85+yrs")
any_85_plus_or



#### Adults over 75 ####
all_data[, .N, keyby = D2_75_84]
all_data[D2_75_84 %in% c("1 person", "2 people", 
                        "3 people", "4 or more people") |
           any_under_6 == TRUE, any_75_plus := TRUE]
all_data[D2_75_84 == "0 people" & any_under_6 == FALSE, any_75_plus := FALSE]
all_data[, .N, keyby = any_75_plus]
any_75_plus <- get_big_saver_characteristics(all_data[!is.na(any_75_plus)], 
                                             "any_75_plus",
                                             "Any aged 75+yrs")
any_75_plus
any_75_plus_or <- get_odds_ratio_data(all_data[!is.na(any_75_plus)], 
                                      "any_75_plus",
                                      "Any aged 75+yrs")
any_75_plus_or




#### Electric Vehicles ####

all_data[, .N, keyby = .(D5)]
all_data[! D5 %in% c("Don't know", "No response"), D5_combo := D5]
all_data[D5_combo %in% c("2", "3 or more"), D5_combo := "2+"]
all_data[, .N, keyby = .(D5_combo)]


all_data[, any_EV := D5 != 0]

all_data[! D5 %in% c("Don't know", "No response"), 
         EV := ifelse(D5 == 0, FALSE, TRUE)]
all_data[, .N, keyby = EV]

EV <- get_big_saver_characteristics(all_data[!is.na(EV)], "EV")


#### solar pv ####
all_data[, .N, keyby = B3_1_yes] # Solar PV

all_data[B3_1_yes != "No response", solar_PV := B3_1_yes]

solar_pv <- get_big_saver_characteristics(all_data[!is.na(solar_PV)], "solar_PV")


#### solar thermal #### 
all_data[, .N, keyby = B3_2_yes] # Solar water heating
all_data[B3_2_yes != "No response",
               solar_water_heating := B3_2_yes]

solar_water_heating <- get_big_saver_characteristics(all_data[!is.na(solar_water_heating)], 
                                                     "solar_water_heating")
solar_water_heating

#### Battery ####
all_data[, .N, keyby = B3_3_yes] # Battery storage
all_data[B3_3_yes != "No response",
               battery := B3_3_yes]
all_data[, .N, keyby = battery] 


#### Sick or disabled ####
all_data[, .N, keyby = .(D3_5, D3_ignored)]
all_data[, .N, keyby = .(D3_5, D3_flag_low)]

all_data[, .N, keyby = D3_4]

all_data[D3_5 %in% c("1 person", "2 people", "3 people",
                     "4 or more people"), any_sick_disabled := TRUE]
all_data[D3_5 %in% c("0 people"), any_sick_disabled := FALSE]

all_data[, .N, keyby = any_sick_disabled]
any_sick_disabled <- get_big_saver_characteristics(all_data[!is.na(any_sick_disabled)], 
                                                   "any_sick_disabled",
                                                   "Any not working due to long-term sickness or disability")
any_sick_disabled
any_sick_disabled_or <- get_odds_ratio_data(all_data[!is.na(any_sick_disabled)], 
                                            "any_sick_disabled",
                                            "Any not working due to long-term sickness or disability")
any_sick_disabled_or
  


#### Mould or damp issues  ####

# Any issues (B5)
all_data[, .N, keyby = B5]
all_data[B5 == "Yes", mould_or_damp := TRUE]
all_data[B5 == "No", mould_or_damp := FALSE]

all_data[, .N, keyby = mould_or_damp]

mould_or_damp <- get_big_saver_characteristics(all_data[!is.na(mould_or_damp)], 
                                               "mould_or_damp",
                                               "Mould or damp")
mould_or_damp
mould_or_damp_or <- get_odds_ratio_data(all_data[!is.na(mould_or_damp)], 
                                        "mould_or_damp",
                                        "Mould or damp")
mould_or_damp_or


# Substantial issues (B6)
all_data[, .N, keyby = B6_1]
all_data[B6_1 == "Substantial" | B6_2 == "Substantial" | B6_3 == "Substantial",
         substantial_mould := TRUE]
all_data[B6_1 %in% c("No", "Minor") | B5 == "No", 
         substantial_mould := FALSE]

all_data[, .N, keyby = substantial_mould]
substantial_mould <- get_big_saver_characteristics(all_data[!is.na(substantial_mould)], 
                                                   "substantial_mould",
                                                   "Substantial mould")
substantial_mould
substantial_mould_or <- get_odds_ratio_data(all_data[!is.na(substantial_mould)], 
                                            "substantial_mould",
                                            "Substantial mould")
substantial_mould_or


# Individual regression ---------------------------------------------------

all_data <- energy_with_totals[variable == "savings_perc", .(PUPRN, total)][all_data, on = "PUPRN"]

set.seed(11)
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)


imd_r <- train(as.formula(total ~ IMD_quintile),
               data = all_data,
               method = "lm",
               preProcess = "scale",
               trControl = control,
               na.action = na.exclude)
summary(imd_r) # X

tenure_r <- train(as.formula(total ~ tenure),
                  data = all_data,
                  method = "lm",
                  preProcess = "scale",
                  trControl = control,
                  na.action = na.exclude)
summary(tenure_r) # social renter **

n_occupants_r <- train(as.formula(total ~ occupants_cts),
                  data = all_data,
                  method = "lm",
                  preProcess = "scale",
                  trControl = control,
                  na.action = na.exclude)
summary(n_occupants_r) # X

financial_wellbeing_r <- train(as.formula(total ~ combo_financial_wellbeing),
                       data = all_data,
                       method = "lm",
                       preProcess = "scale",
                       trControl = control,
                       na.action = na.exclude)
summary(financial_wellbeing_r) # low ***

any_children_r <- train(as.formula(total ~ any_children),
                               data = all_data,
                               method = "lm",
                               preProcess = "scale",
                               trControl = control,
                               na.action = na.exclude)
summary(any_children_r) # X

any_under_6_r <- train(as.formula(total ~ any_under_6),
                        data = all_data,
                        method = "lm",
                        preProcess = "scale",
                        trControl = control,
                        na.action = na.exclude)
summary(any_under_6_r) # X

any_75_plus_r <- train(as.formula(total ~ any_75_plus),
                       data = all_data,
                       method = "lm",
                       preProcess = "scale",
                       trControl = control,
                       na.action = na.exclude)
summary(any_75_plus_r) # X

any_85_plus_r <- train(as.formula(total ~ any_85_plus),
                       data = all_data,
                       method = "lm",
                       preProcess = "scale",
                       trControl = control,
                       na.action = na.exclude)
summary(any_85_plus_r) # *

dwelling_r <- train(as.formula(total ~ dwelling),
                       data = all_data,
                       method = "lm",
                       preProcess = "scale",
                       trControl = control,
                       na.action = na.exclude)
summary(dwelling_r) # flat ***

house_age_r <- train(as.formula(total ~ house_age),
                    data = all_data,
                    method = "lm",
                    preProcess = "scale",
                    trControl = control,
                    na.action = na.exclude)
summary(house_age_r) # X

EPC_rating_r <- train(as.formula(total ~ EPC_rating),
                     data = all_data,
                     method = "lm",
                     preProcess = "scale",
                     trControl = control,
                     na.action = na.exclude)
summary(EPC_rating_r) # E-G ***, D **

bedrooms_r <- train(as.formula(total ~ bedrooms_cts),
                      data = all_data,
                      method = "lm",
                      preProcess = "scale",
                      trControl = control,
                      na.action = na.exclude)
summary(bedrooms_r) # 1 bedroom **

solar_PV_r <- train(as.formula(total ~ solar_PV),
                    data = all_data,
                    method = "lm",
                    preProcess = "scale",
                    trControl = control,
                    na.action = na.exclude)
summary(solar_PV_r) # X

solar_water_heating_r <- train(as.formula(total ~ solar_water_heating),
                    data = all_data,
                    method = "lm",
                    preProcess = "scale",
                    trControl = control,
                    na.action = na.exclude)
summary(solar_water_heating_r) # X

battery_r <- train(as.formula(total ~ battery),
                               data = all_data,
                               method = "lm",
                               preProcess = "scale",
                               trControl = control,
                               na.action = na.exclude)
summary(battery_r) # X

EV_r <- train(as.formula(total ~ EV),
                   data = all_data,
                   method = "lm",
                   preProcess = "scale",
                   trControl = control,
                   na.action = na.exclude) 
summary(EV_r) # X



# Regression on significant variables from individual regression ----------

combined_indiv_reg <- train(
  as.formula(
    total ~
      dwelling +
      EPC_rating +
      bedrooms_cts +
      tenure +
      combo_financial_wellbeing +
      any_85_plus
  ),
  data = all_data,
  method = "lm",
  preProcess = "scale",
  trControl = control,
  na.action = na.exclude
)
summary(combined_indiv_reg)
characteristics_reg <- data.table(tidy(combined_indiv_reg))
characteristics_reg[, N := 1208]

# Fuel poverty and underheating -------------------------------------------

all_data[, .N, keyby = combo_financial_wellbeing]
all_data[, .N, keyby = E1]
all_data[, .N, keyby = D4_orig]

# New data
all_data[, financial_wellbeing := E1]
all_data[financial_wellbeing %in% c("Don't know", "Prefer not to say"),
         financial_wellbeing := "Don't know or prefer not to say"]
all_data[, .N, keyby = financial_wellbeing]
new_survey_financial_wellbeing <- all_data[!is.na(financial_wellbeing) & 
                                             financial_wellbeing != "No response", 
                                           .N, keyby = financial_wellbeing]
new_survey_financial_wellbeing[, tot := new_survey_financial_wellbeing[, sum(N)]]
new_survey_financial_wellbeing[, perc := round(N / tot * 100, 3)]


old_finance_wellbeing_key <- data.table(D4_orig = c(-3, -1, 1:5),
                                        prev_financial_wellbeing = c("Don't know or prefer not to say",
                                                                "Don't know or prefer not to say",
                                                                "Living comfortably",
                                                                "Doing alright",
                                                                "Just about getting by",
                                                                "Finding it quite difficult",
                                                                "Finding it very difficult"))
all_data <- old_finance_wellbeing_key[all_data, on = "D4_orig"]
all_data[, .N, keyby = prev_financial_wellbeing]

old_survey_financial_wellbeing <- all_data[!is.na(prev_financial_wellbeing)
                                           , .N, keyby = prev_financial_wellbeing]
old_survey_financial_wellbeing[, tot := old_survey_financial_wellbeing[, sum(N)]]
old_survey_financial_wellbeing[, perc := round(N / tot * 100, 3)]


all_data[, prev_financial_wellbeing := factor(prev_financial_wellbeing,
                                              levels = c("Living comfortably",
                                                         "Doing alright",
                                                         "Just about getting by",
                                                         "Finding it quite difficult",
                                                         "Finding it very difficult",
                                                         "Don't know or prefer not to say"))]
all_data[, financial_wellbeing := factor(financial_wellbeing,
                                              levels = c("Living comfortably",
                                                         "Doing alright",
                                                         "Just about getting by",
                                                         "Finding it quite difficult",
                                                         "Finding it very difficult",
                                                         "Don't know or prefer not to say"))]

fw_change_counts <- all_data[!is.na(prev_financial_wellbeing) & 
           !is.na(financial_wellbeing), .N, 
         keyby = .(prev_financial_wellbeing, financial_wellbeing)]
fw_change_counts

library(ggalluvial)
ggplot(fw_change_counts[!prev_financial_wellbeing %in% "Don't know or prefer not to say" & 
                          !financial_wellbeing %in% c("Don't know or prefer not to say",
                                                      "No response")],
       aes(axis1 = prev_financial_wellbeing,
           axis2 = financial_wellbeing,
           y = N)) + 
  geom_alluvium(aes(fill = financial_wellbeing)) + 
  geom_stratum() + 
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("prev_financial_wellbeing",
                              "financial_wellbeing"),
                   expand = c(0.15, 0.05))

fw_change_counts[!prev_financial_wellbeing %in% "Don't know or prefer not to say" & 
                   !financial_wellbeing %in% c("Don't know or prefer not to say",
                                               "No response")]
reduced_fw_change_counts <- fw_change_counts[!prev_financial_wellbeing %in% "Don't know or prefer not to say" & 
                                               !financial_wellbeing %in% c("Don't know or prefer not to say",
                                                                           "No response") & 
                                               N >= 10]

ggplot(reduced_fw_change_counts,
       aes(axis1 = prev_financial_wellbeing,
           axis2 = financial_wellbeing,
           y = N)) + 
  geom_alluvium(aes(fill = financial_wellbeing)) + 
  geom_stratum() + 
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("prev_financial_wellbeing",
                              "financial_wellbeing"),
                   expand = c(0.15, 0.05))




# probability of being in low financial wellbeing if big saver compared with not being biggest saver
financial_wellbeing
financial_wellbeing_rr <- calc_risk_ratio_big_saver_given_characteristics("combo_financial_wellbeing")

ease_meeting_heating_costs

meeting_heating_costs_dt <- all_data[combo_C5 %in% c("Very or fairly easy",
                                                     "Very or fairly difficult"),
                                     .(group, combo_C5)]
meeting_heating_costs_dt[, combo_C5 := factor(combo_C5, 
                                              levels = c("Very or fairly easy",
                                                         "Very or fairly difficult"))]
ease_meeting_heating_costs_rr <- calc_risk_ratio_big_saver_given_characteristics("combo_C5",
                                                                                 dt = meeting_heating_costs_dt,
                                                                                 name = "ease_meeting_heating_costs")


keep_comfortably_warm
keep_comfortably_warm_rr <- calc_risk_ratio_big_saver_given_characteristics("C3_combo",
                                                                            name = "keep_comfortably_warm")


mould_or_damp
mould_or_damp_rr <- calc_risk_ratio_big_saver_given_characteristics("mould_or_damp")

substantial_mould
substantial_mould_rr <- calc_risk_ratio_big_saver_given_characteristics("substantial_mould")


fuel_poverty_underheating_counts <- rbind(financial_wellbeing,
                                          ease_meeting_heating_costs,
                                          keep_comfortably_warm,
                                          mould_or_damp,
                                          substantial_mould)

fuel_poverty_underheating_risk_ratios <- rbind(financial_wellbeing_rr,
                                               ease_meeting_heating_costs_rr,
                                               keep_comfortably_warm_rr,
                                               mould_or_damp_rr,
                                               substantial_mould_rr)
ggplot(fuel_poverty_underheating_counts,
       aes(x = subgroup,
           y = perc,
           fill = group)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(.~variable) + 
  coord_flip()

ggplot(fuel_poverty_underheating_risk_ratios[saver_group != "other" & 
                                               p.value < 0.05],
       aes(x = variable,
           y = riskratio * 100 - 100)) + 
  geom_bar(stat = "identity", position = "dodge")



# unable to keep comfortably warm logit -----------------------------------

energy_with_totals_wide <- dcast(energy_with_totals,
                                 PUPRN ~ variable,
                                 value.var = c("elec", "gas", "total"))

all_data[, .N, keyby = C3_combo]
keeping_warm_input_data <- energy_with_totals_wide[all_data[!is.na(C3_combo), 
                                                            .(PUPRN, C3_combo,
                                                              EPC_rating)],
                                                   on = "PUPRN"]



set.seed(11)
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

keep_comfortably_warm_gas_logit <- train(
  as.formula(
    C3_combo ~
      gas_predicted_energy_kWh +
      gas_observed_energy_kWh +
      gas_savings_perc + 
      EPC_rating),
  data = keeping_warm_input_data,
  method = "glm",
  preProcess = "scale",
  trControl = control,
  na.action = na.exclude
)
summary(keep_comfortably_warm_gas_logit)

keep_comfortably_warm_total_logit <- train(
  as.formula(
    C3_combo ~
      total_predicted_energy_kWh +
      total_observed_energy_kWh +
      total_savings_perc),
  data = keeping_warm_input_data,
  method = "glm",
  preProcess = "scale",
  trControl = control,
  na.action = na.exclude
)
summary(keep_comfortably_warm_total_logit)

tmp <- train(as.formula(C3_combo ~ EPC_rating),
  data = keeping_warm_input_data,
  method = "glm",
  preProcess = "scale",
  trControl = control,
  na.action = na.exclude
)
summary(tmp)

tmp <- train(
  as.formula(
    C3_combo ~ total_savings_perc),
  data = keeping_warm_input_data,
  method = "glm",
  preProcess = "scale",
  trControl = control,
  na.action = na.exclude
)
summary(tmp)

tmp <- train(
  as.formula(
    C3_combo ~ total_observed_energy_kWh),
  data = keeping_warm_input_data,
  method = "glm",
  preProcess = "scale",
  trControl = control,
  na.action = na.exclude
)
summary(tmp)

tmp <- train(
  as.formula(
    C3_combo ~ total_predicted_energy_kWh),
  data = keeping_warm_input_data,
  method = "glm",
  preProcess = "scale",
  trControl = control,
  na.action = na.exclude
)
summary(tmp)

keep_comfortably_warm_total_logit <- train(
  as.formula(
    C3_combo ~
      total_observed_energy_kWh +
      total_savings_perc),
  data = keeping_warm_input_data,
  method = "glm",
  preProcess = "scale",
  trControl = control,
  na.action = na.exclude
)
summary(keep_comfortably_warm_total_logit)


# Save --------------------------------------------------------------------

fwrite(characteristics_reg,
       file = "./data/outputs/rq6/rq6_characteristics_regression.csv")

fwrite(reduced_fw_change_counts,
       file = "./data/outputs/rq6/rq6_change_in_financial_wellbeing.csv")

fwrite(fuel_poverty_underheating_counts,
       file = "./data/outputs/rq6/rq6_fuel_poverty_underheating_counts.csv")

fwrite(fuel_poverty_underheating_risk_ratios,
       file = "./data/outputs/rq6/rq6_fuel_poverty_underheating_risk_ratios.csv")