# RQ4 - correlation between energy-saving behaviours and energy savings


library(data.table)
library(ggplot2)
library(mlbench) # for feature selection
library(caret) # for feature selection
library(stringr)
library(broom)
library(epitools)

source("./scripts/functions/get_action_change_by_quintile.R")
source("./scripts/functions/calc_risk_ratio_big_saver_given_action.R")
source("./scripts/functions/tidy.train.R")


# import 2023 survey
colc_survey <- readRDS("S:/SERL_Frontier1/EZW/data/survey/COL_survey_processed.rds")
energy_results <- readRDS("S:/SERL_Frontier1/EZW/data/analysis_data/intermediary/individual_winter_savings.RDS")

colc_survey <- energy_results[variable == "observed_energy_kWh" & fuel == "elec", 
                              .(PUPRN, saving_perc_quintile, tot_obs_quintile)][colc_survey, on = "PUPRN"]
colc_survey <- colc_survey[!is.na(saving_perc_quintile)] 

thermostat <- readRDS("./data/survey/thermostat.RDS")



# Count number of action change types since last winter -------------------


A6 <- colc_survey[, .SD, .SDcols = c("PUPRN", "saving_perc_quintile",
                                     paste0("A6_", 1:16))]

A6_long <- data.table::melt(A6, id.vars = c("PUPRN", "saving_perc_quintile"))
n_behaviours <- A6_long[!is.na(value) & value != "", 
                        .N, keyby = .(PUPRN, saving_perc_quintile, value)]


n_behaviours_by_saver_percentile <- n_behaviours[, .(total_number_actions = sum(N)), 
                                                 keyby = .(value,
                                                           saving_perc_quintile)]
N_households_actions_A6 <- A6[, .N, keyby = saving_perc_quintile]

n_behaviours_by_saver_percentile <- N_households_actions_A6[n_behaviours_by_saver_percentile,
                                                            on = "saving_perc_quintile"]
n_behaviours_by_saver_percentile[, mean_n_actions := total_number_actions / N]

n_behaviours_by_saver_percentile[, value := factor(value,
                                                   levels = c("No response",
                                                              "Not applicable, cannot do this",
                                                              "A lot less",
                                                              "A little less",
                                                              "About the same",
                                                              "A little more",
                                                              "A lot more"))]
n_behaviours_by_saver_percentile[, N_PUPRN := length(unique(n_behaviours$PUPRN))]


ggplot(n_behaviours_by_saver_percentile[!is.na(value)],
       aes(x = value,
           y = mean_n_actions,
           fill = factor(saving_perc_quintile))) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_grey()




# Overall action popularity -----------------------------------------------

# In general, what percentage of the population report doing it 'always' or doing it if yes/no

# A2 Living spaces that aren't normally heated
A2 <- colc_survey[, .N, keyby = A2]
A2 <- A2[A2 != "No response"]
A2[, `:=`(tot = sum(N),
          perc = N / sum(N) * 100,
          variable = "A2")]
setnames(A2, "A2", "response")
A2
setcolorder(A2, c("variable", "response", "N", "tot", "perc"))
A2 <- A2[response == "Yes"]
A2

# A5: 16 behaviours
A5_all <- colc_survey[, .SD, .SDcols = c("PUPRN", paste0("A5_", 1:16))]
A5_long <- data.table::melt(A5_all, id.vars = "PUPRN")

A5 <- A5_long[value == "Always", .N, keyby = variable]
A5_responses <- A5_long[!is.na(value) & !value %in% c("No response",
                                                      "Not applicable, cannot do this"), 
                        .(tot = .N), 
                        keyby = variable]
A5 <- A5_responses[A5, on = "variable"]


A5[, `:=`(perc = N / tot * 100,
          response = "Always")]
setcolorder(A5, c("variable", "response", "N", "tot", "perc"))
A5


# A7 Adjust heating when away for a few days 
A7 <- colc_survey[, .N, keyby = A7]
A7 <- A7[!A7 %in% c("No response", "Not applicable")]
A7[, `:=`(tot = sum(N),
          perc = N / sum(N) * 100,
          variable = "A7")]
setnames(A7, "A7", "response")
A7
setcolorder(A7, c("variable", "response", "N", "tot", "perc"))
A7 <- A7[response == "Always"]
A7


# A12 amount of effort made
A12 <- colc_survey[, .N, keyby = A12]
A12
A12 <- A12[!A12 %in% c("No response", "Don't know")]
A12[, `:=`(tot = sum(N),
          perc = N / sum(N) * 100,
          variable = "A12")]
setnames(A12, "A12", "response")
A12
setcolorder(A12, c("variable", "response", "N", "tot", "perc"))
effort_made <- A12
A12 <- A12[response == "A great deal of effort"]
A12


overall_action_popularity <- rbind(A2, A5, A7, A12, 
                                   effort_made[response !="A great deal of effort"])



# Comparing changes since winter across saving quintiles ------------------

# A2 Living spaces that aren't normally heated
colc_survey[, .N, keyby = A2]
A2_change <- get_action_change_by_quintile(action = "A2",
                                           valid_responses = c("Yes", "No"),
                                           single_response = "Yes")


# A3 Reduced boiler flow temperature
colc_survey[, .N, keyby = A3]
A3_change <- get_action_change_by_quintile(action = "A3",
                                           valid_responses = c("Yes", "No"),
                                           single_response = "Yes")

# A4 heating house for fewer hours
colc_survey[, .N, keyby = A4]
A4_change <- get_action_change_by_quintile(action = "A4",
                                           valid_responses = c("Yes", "No"),
                                           single_response = "Yes")

# A5: 16 behaviours
colc_survey[, .N, keyby = A6_1]
A6_change <- get_action_change_by_quintile(action = "A6_1",
                                           valid_responses = c("A lot less",
                                                               "A little less",
                                                               "About the same",
                                                               "A little more",
                                                               "A lot more"),
                                           single_response = "A lot more")
for(i in 2:16) {
  A6_change <- rbind(A6_change,
                     get_action_change_by_quintile(action = paste0("A6_", i),
                                                   valid_responses = c("A lot less",
                                                                       "A little less",
                                                                       "About the same",
                                                                       "A little more",
                                                                       "A lot more"),
                                                   single_response = "A lot more"))
}

# A7 Adjust heating when away for a few days 
colc_survey[, .N, keyby = A7]
A7_change <- get_action_change_by_quintile(action = "A7",
                                           valid_responses = c("Never",
                                                               "Not very often",
                                                               "Quite often",
                                                               "Very often",
                                                               "Always"),
                                           single_response = "Always")

# A11 use of smart meter IHD
colc_survey[, .N, keyby = A11]
A11_change <- get_action_change_by_quintile(action = "A11",
                                            valid_responses = c("About the same",
                                                                "Less often",
                                                                "More often"),
                                            single_response = "More often")

# A12 making a lot of effort to reduce consumption
colc_survey[, .N, keyby = A12]
A12_change <- get_action_change_by_quintile(action = "A12",
                                            valid_responses = c("A great deal of effort",
                                                                "A little effort",
                                                                "No effort at all",
                                                                "Some effort"),
                                            single_response = "A great deal of effort")

behaviour_change_quintile_perc <- rbind(A2_change,
                                        A3_change,
                                        A4_change,
                                        A6_change,
                                        A7_change,
                                        A11_change,
                                        A12_change)

# Behaviour change risk ratios --------------------------------------------

colc_survey[, is_saver_q5 := saving_perc_quintile == 5]

A2_risk <- calc_risk_ratio_big_saver_given_action(action = "A2",
                                                  valid_responses = c("Yes", "No"),
                                                  single_response = "Yes")
A3_risk <- calc_risk_ratio_big_saver_given_action(action = "A3",
                                                  valid_responses = c("Yes", "No"),
                                                  single_response = "Yes")
A4_risk <- calc_risk_ratio_big_saver_given_action(action = "A4",
                                                  valid_responses = c("Yes", "No"),
                                                  single_response = "Yes")

A6_risk <- calc_risk_ratio_big_saver_given_action(action = "A6_1",
                                                  valid_responses = c("A lot less",
                                                                      "A little less",
                                                                      "About the same",
                                                                      "A little more",
                                                                      "A lot more"),
                                                  single_response = "A lot more")
for(i in 2:16) {
  A6_risk <- rbind(A6_risk,
                   calc_risk_ratio_big_saver_given_action(action = paste0("A6_", i),
                                                          valid_responses = c("A lot less",
                                                                              "A little less",
                                                                              "About the same",
                                                                              "A little more",
                                                                              "A lot more"),
                                                          single_response = "A lot more"))
}

A7_risk <- calc_risk_ratio_big_saver_given_action(action = "A7",
                                                  valid_responses = c("Never",
                                                                      "Not very often",
                                                                      "Quite often",
                                                                      "Very often",
                                                                      "Always"),
                                                  single_response = "Always")
A11_risk <- calc_risk_ratio_big_saver_given_action(action = "A11",
                                                   valid_responses = c("About the same",
                                                                       "Less often",
                                                                       "More often"),
                                                   single_response = "More often")
A12_risk <- calc_risk_ratio_big_saver_given_action(action = "A12",
                                                  valid_responses = c("A great deal of effort",
                                                                      "A little effort",
                                                                      "No effort at all",
                                                                      "Some effort"),
                                                  single_response = "A great deal of effort")

behaviour_change_risk_ratios <- rbind(A2_risk,
                                      A3_risk,
                                      A4_risk,
                                      A6_risk,
                                      A7_risk,
                                      A11_risk,
                                      A12_risk)
behaviour_change_risk_ratios[, min(N_big_savers)]



# Actions regression data prep --------------------------------------------

colc_survey[A2 %in% c("Yes", "No"), 
            A2_binary := factor(A2, levels = c("No", "Yes"))]

colc_survey[A3 == "Yes", A3_binary := "Yes"]
colc_survey[A3 %in% c("No", "Not applicable"), 
            A3_binary := "No or not applicable"]
colc_survey[, A3_binary := factor(A3_binary, levels = c("No or not applicable",
                                                        "Yes"))]
colc_survey[A4 %in% c("Yes", "No"), 
            A4_binary := factor(A4, levels = c("No", "Yes"))]


colc_survey[A7 == "Always", A7_binary := A7]
colc_survey[A7 %in% c("Never",
                      "Not very often",
                      "Quite often",
                      "Very often"), A7_binary := "Not always"]
colc_survey[, A7_binary := factor(A7_binary, levels = c("Not always", "Always"))]
colc_survey[, .N, keyby = A7_binary]


colc_survey[A11 == "More often", A11_binary := "More often"]
colc_survey[!A11 %in% c("No response", "Don't know", "More often"), 
            A11_binary := "Less often/same/not possible"]
colc_survey[, A11_binary := factor(A11_binary, levels = c("Less often/same/not possible", 
                                                          "More often"))]
colc_survey[, .N, keyby = A11_binary]


# A6 actions
A6_wide <- colc_survey[, .SD, .SDcols = c("PUPRN", "saving_perc_quintile",
                                     paste0("A6_", 1:16))]

A6_long <- data.table::melt(A6_wide, id.vars = c("PUPRN", "saving_perc_quintile"))
A6_long[, unique(value)]
A6_long[value == "A lot more", binary := "A lot more"]
A6_long[!value %in% c("No response", "", "A lot more") & 
          !is.na(value), binary := "Not a lot more"]
A6_long[, binary := factor(binary, levels = c("Not a lot more", "A lot more"))]

any_A6_na <- A6_long[is.na(binary), unique(PUPRN)]

A6_for_regression <- dcast(A6_long[!PUPRN %in% any_A6_na, .(PUPRN, variable, binary)],
                           PUPRN ~ variable,
                           value.var = "binary")

regression_data <- colc_survey[, .(PUPRN, A2_binary, A3_binary, A4_binary, 
                                   A7_binary, A11_binary)][A6_for_regression, 
                                                           on = "PUPRN"]

regression_data <- regression_data[!is.na(A2_binary) & 
                                     !is.na(A3_binary) &
                                     !is.na(A4_binary) & 
                                     !is.na(A7_binary) &
                                     !is.na(A11_binary)]

regression_data <- energy_results[variable == "savings_perc" & 
                                    fuel == "elec", .(PUPRN, value)][regression_data,
                                                                     on = "PUPRN"]
setnames(regression_data, "value", "elec_saving_perc")

regression_data <- energy_results[variable == "savings_perc" 
                                  & fuel == "gas", .(PUPRN, value)][regression_data,
                                                                    on = "PUPRN"]
setnames(regression_data, "value", "gas_saving_perc")



# Electricity % saving regression -----------------------------------------

control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(11)
elec_actions_perc_lm <- train(as.formula(paste0("elec_saving_perc ~ A2_binary + A3_binary + 
                                                A4_binary + A7_binary + A11_binary + ",
                                                paste(paste0("A6_", 1:16), collapse = " + "))),
                              data = regression_data,
                              method = "lm",
                              preProcess = "scale",
                              trControl = control)
summary(elec_actions_perc_lm)
importance <- varImp(elec_actions_perc_lm, scale = FALSE)
plot(importance)

elec_importance <- data.table(var = rownames(varImp(elec_actions_perc_lm, scale = FALSE)$importance),
                              importance = varImp(elec_actions_perc_lm, scale = FALSE)$importance)
data.table::setorder(elec_importance, cols = -"importance.Overall")


# TOP 10
elec_top_vars <- elec_importance[1:10, str_remove(var, "A lot more")]
elec_top_vars <- str_remove(elec_top_vars, "Yes")
elec_top_vars <- str_remove(elec_top_vars, "More often")
elec_top_vars <- str_remove_all(elec_top_vars, "`")
elec_top_vars

set.seed(11)
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
elec_model_top_10 <- train(as.formula("elec_saving_perc ~."),
                          data = regression_data[, .SD,
                                                 .SDcols = c("elec_saving_perc", elec_top_vars)],
                          method = "lm",
                          preProcess = "scale",
                          trControl = control,
                          na.action = na.exclude)
elec_model_importance_top_10 <- varImp(elec_model_top_10, scale = FALSE)
plot(elec_model_importance_top_10)
summary(elec_model_top_10) 


elec_model_top_10 <- data.table(tidy(elec_model_top_10))
elec_model_top_10[, name := "elec_model_top_10_lm"]
elec_model_top_10[, N_households := nrow(regression_data)]



# Gas % saving regression ------------------------------------------------

control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(11)
gas_actions_perc_lm <- train(as.formula(paste0("gas_saving_perc ~ A2_binary + A3_binary + 
                                                A4_binary + A7_binary + A11_binary + ",
                                                paste(paste0("A6_", 1:16), collapse = " + "))),
                              data = regression_data,
                              method = "lm",
                              preProcess = "scale",
                              trControl = control)
summary(gas_actions_perc_lm)
importance <- varImp(gas_actions_perc_lm, scale = FALSE)
plot(importance)

gas_importance <- data.table(var = rownames(varImp(gas_actions_perc_lm, scale = FALSE)$importance),
                              importance = varImp(gas_actions_perc_lm, scale = FALSE)$importance)
data.table::setorder(gas_importance, cols = -"importance.Overall")


# TOP 10
gas_top_vars <- gas_importance[1:10, str_remove(var, "A lot more")]
gas_top_vars <- str_remove(gas_top_vars, "Yes")
gas_top_vars <- str_remove(gas_top_vars, "More often")
gas_top_vars <- str_remove_all(gas_top_vars, "`")
gas_top_vars

set.seed(11)
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
gas_model_top_10 <- train(as.formula("gas_saving_perc ~."),
                           data = regression_data[, .SD,
                                                  .SDcols = c("gas_saving_perc", gas_top_vars)],
                           method = "lm",
                           preProcess = "scale",
                           trControl = control,
                           na.action = na.exclude)
gas_model_importance_top_10 <- varImp(gas_model_top_10, scale = FALSE)
plot(gas_model_importance_top_10)
summary(gas_model_top_10) 


gas_model_top_10 <- data.table(tidy(gas_model_top_10))
gas_model_top_10[, name := "gas_model_top_10_lm"]
gas_model_top_10[, N_households := nrow(regression_data)]


# Combine
behaviours_lms <- rbind(elec_model_top_10,
                        gas_model_top_10)
setcolorder(behaviours_lms, "name")
behaviours_lms[p.value < 0.001, sig := "***"]
behaviours_lms[p.value < 0.01 & p.value >= 0.001, sig := "**"]
behaviours_lms[p.value < 0.05 & p.value >= 0.01, sig := "*"]



# Thermostat settings -----------------------------------------------------

thermostat <- data.table(thermostat)
thermostat[1:4, .(PUPRN, S1_A5_degC, S3_A1_corr_C, deltaSetting)]

thermostat_filtered <- energy_results[variable == "savings_perc" & 
                                        fuel == "gas",
                             .(PUPRN, 
                               saving_perc_quintile, 
                               tot_obs_quintile,
                               value)][thermostat[, .(PUPRN, S1_A5_degC, 
                                                                 S3_A1_corr_C, deltaSetting)],
                                                                               on = "PUPRN"]
setnames(thermostat_filtered, "value", "gas_savings_perc")

thermostat_filtered <- thermostat_filtered[!is.na(saving_perc_quintile)]

# check for outliers
thermostat_filtered[S1_A5_degC <= 5] 
thermostat_filtered[S1_A5_degC >= 35] 
thermostat_filtered[S3_A1_corr_C <= 5] 
thermostat_filtered[S3_A1_corr_C >= 35] 
thermostat_filtered <- thermostat_filtered[S1_A5_degC > 5 & S1_A5_degC < 35 & 
                                             S3_A1_corr_C > 5 & S3_A1_corr_C < 35]

thermostat_by_saver_quintile <- thermostat_filtered[, .(mean_orig_temp = mean(S1_A5_degC),
                                                        mean_new_temp = mean(S3_A1_corr_C),
                                                        mean_change = mean(deltaSetting)),
                                                    keyby = saving_perc_quintile]

thermostat_filtered[, .(mean_orig_temp = mean(S1_A5_degC),
                        mean_new_temp = mean(S3_A1_corr_C),
                        mean_change = mean(deltaSetting)),
                    keyby = tot_obs_quintile]

long_thermostat_saving_quintile <- melt(thermostat_by_saver_quintile,
                                        id.vars = "saving_perc_quintile")

long_thermostat <- melt(thermostat_filtered,
                        id.vars = c("PUPRN", "saving_perc_quintile", "tot_obs_quintile"))

ggplot(long_thermostat[variable != "deltaSetting" & saving_perc_quintile %in% c(1, 5)]) +
  geom_histogram(aes(x = value,
                     fill = variable),
                 position = "dodge") +
  facet_grid(.~saving_perc_quintile)


t <- ggplot(long_thermostat[variable == "deltaSetting" & saving_perc_quintile %in% c(1, 5)]) +
  geom_histogram(aes(x = value,
                     fill = factor(saving_perc_quintile)),
                 position = "dodge")

t <- ggplot(long_thermostat[variable == "deltaSetting" & 
                              saving_perc_quintile %in% c(1, 5) & 
                              value <= 5 & value >= -5]) +
  geom_histogram(aes(x = value,
                     fill = factor(saving_perc_quintile)),
                 position = "dodge")
t

temp_stats_by_quintile <- long_thermostat[, .(mean = mean(value),
                                  sd = sd(value),
                                  N = .N), 
                keyby = .(variable, saving_perc_quintile)]
temp_stats_by_quintile[, sem := sd / sqrt(N)]

temp_stats_all <- long_thermostat[, .(mean = mean(value),
                                      sd = sd(value),
                                      N = .N), 
                                  keyby = .(variable)]
temp_stats_all[, `:=`(sem = sd / sqrt(N),
                      saving_perc_quintile = 0)]

temp_stats_14 <- long_thermostat[saving_perc_quintile <= 4, 
                                 .(mean = mean(value),
                                      sd = sd(value),
                                      N = .N), 
                                  keyby = .(variable)]
temp_stats_14[, `:=`(sem = sd / sqrt(N),
                      saving_perc_quintile = 14)]

temp_stats <- rbind(temp_stats_all, temp_stats_14, temp_stats_by_quintile)


ggplot(temp_stats[saving_perc_quintile %in% 1:5],
       aes(x = factor(saving_perc_quintile),
           y = mean)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(.~variable)


# Regression
set.seed(11)
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

thermostat_change_lm <- train(as.formula(gas_savings_perc ~ deltaSetting),
                              data = thermostat_filtered,
                              method = "lm",
                              preProcess = "scale",
                              trControl = control)
summary(thermostat_change_lm)

thermostat_regression <- data.table(tidy(thermostat_change_lm))
thermostat_regression[, N := nrow(thermostat_filtered)]

# Export ------------------------------------------------------------------


fwrite(n_behaviours_by_saver_percentile,
       "./data/outputs/rq4/rq4_n_behaviours_by_saver_percentile.csv")


fwrite(overall_action_popularity,
       "./data/outputs/rq4/rq4_overall_action_popularity.csv")

fwrite(behaviour_change_quintile_perc,
       "./data/outputs/rq4/rq4_behaviour_change_quintile_perc.csv")

fwrite(behaviour_change_risk_ratios,
       "./data/outputs/rq4/rq4_behaviour_change_risk_ratios.csv")

fwrite(temp_stats,
       "./data/outputs/rq4/rq4_thermostat_stats.csv")

fwrite(thermostat_regression,
       "./data/outputs/rq4/rq4_thermostat_regression.csv")

fwrite(behaviours_lms,
       "./data/outputs/rq4/rq4_linear_model_actions.csv")

