# Section 3.2.1 in the paper


# setup -------------------------------------------------------------------

source("./scripts/setup_colc.R")
source("./scripts/functions/calc_sdc_percentiles.R")

library(lubridate)

cf <- readRDS("./data/model_training_outputs/all_counterfactuals/cf_with_costs.RDS")

training <- readRDS(paste0("./data/processed/", data_creation_date, "/all_training_with_costs.RDS"))

head(cf)
head(training)

cf[, length(unique(PUPRN))]
training[, length(unique(PUPRN))]

cf[, days_in_month := days_in_month(Read_date_effective_local)]
training[, days_in_month := days_in_month(Read_date_effective_local)]

energy_results <- readRDS("./data/analysis_data/intermediary/individual_winter_savings.RDS")



# Mean monthly values -----------------------------------------------------

this_winter_monthly_indiv <- cf[variable != "obs_min_pred_perc", 
                                .(total_monthly_cost_pounds = mean(daily_cost_pounds) * mean(days_in_month)),
                                keyby = .(PUPRN, fuel, month, variable)]

this_winter_monthly_indiv[, mean(total_monthly_cost_pounds), keyby = .(variable, month, fuel)]


prev_winter_monthly_indiv <- training[, .(total_monthly_cost_pounds = mean(daily_cost_pounds) * mean(days_in_month)),
                                      keyby = .(PUPRN, fuel, month)]

prev_winter_monthly_indiv[, mean(total_monthly_cost_pounds), keyby = .(month, fuel)]



# Add percentage savings --------------------------------------------------

wide_this_winter_monthly_indiv <- dcast(this_winter_monthly_indiv, 
                                        fuel + PUPRN + month ~ variable, 
                                        value.var = "total_monthly_cost_pounds")
wide_this_winter_monthly_indiv[, savings_perc := savings_kWh / predicted_energy_kWh * 100]
this_winter_monthly_indiv <- melt(wide_this_winter_monthly_indiv, 
                                  id.vars = c("fuel", "PUPRN", "month"),
                                  measure.vars = c("observed_energy_kWh",
                                                   "predicted_energy_kWh",
                                                   "savings_kWh",
                                                   "savings_perc"))


# Combine price and change in use -----------------------------------------

perc_changes <- this_winter_monthly_indiv[variable == "savings_perc"]
Oct21_Mar22_costs <- unique(training, by = c("PUPRN", "fuel", "unit_cost", "standing_charge"))

setnames(Oct21_Mar22_costs, 
         c("unit_cost", "standing_charge"),
         c("prev_unit_cost", "prev_standing_charge"))
perc_changes <- Oct21_Mar22_costs[, .(PUPRN, fuel, prev_unit_cost, prev_standing_charge)][
  perc_changes, on = c("PUPRN", "fuel")
]
setnames(perc_changes, "value", "energy_reduction_perc")
perc_changes[, variable := NULL]


perc_changes2 <- unique(cf[, .(PUPRN, fuel, month, unit_cost, standing_charge)])[
  perc_changes, on = c("PUPRN", "fuel", "month")
]

perc_changes2[,`:=`(unit_cost_perc_change = (unit_cost - prev_unit_cost) / prev_unit_cost * 100, 
                  standing_charge_perc_change = (standing_charge - prev_standing_charge) / prev_standing_charge * 100)]
perc_changes2[, price_elasticity_unit_cost := -energy_reduction_perc / unit_cost_perc_change]



# Individual price elasticities -------------------------------------------

indiv_elast_winter <- perc_changes2[, mean(price_elasticity_unit_cost),
                                    keyby = .(PUPRN, fuel)]


# Aggregate stats ---------------------------------------------------------

price_elast_by_month <- perc_changes2[, .(mean_price_elast = mean(price_elasticity_unit_cost),
                                         median_price_elast = median(price_elasticity_unit_cost),
                                         sd_price_elast = sd(price_elasticity_unit_cost),
                                         N = .N),
                                     keyby = .(fuel, month)]



avg_winter_price_elast <-
  indiv_elast_winter[, .(
    mean_price_elast = mean(V1),
    median_price_elast = calc_sdc_percentiles(V1,
                                              percentiles = 0.5,
                                              vector_out = TRUE),
    low_iqr = calc_sdc_percentiles(V1,
                                   percentiles = 0.25,
                                   vector_out = TRUE),
    up_iqr = calc_sdc_percentiles(V1,
                                  percentiles = 0.75,
                                  vector_out = TRUE),
    sd_price_elast = sd(V1),
    N = .N),
  keyby = .(fuel)]



# By saver quintile -------------------------------------------------------

saver_quintiles <- unique(energy_results, by = c("PUPRN", "saving_perc_quintile"))

indiv_elast_winter <- saver_quintiles[, .(PUPRN, saving_perc_quintile)][indiv_elast_winter, on = "PUPRN"]

avg_winter_price_elast_by_quintile <- indiv_elast_winter[, .(
  mean_price_elast = mean(V1),
  median_price_elast = calc_sdc_percentiles(V1,
                                            percentiles = 0.5,
                                            vector_out = TRUE),
  low_iqr = calc_sdc_percentiles(V1,
                                 percentiles = 0.25,
                                 vector_out = TRUE),
  up_iqr = calc_sdc_percentiles(V1,
                                percentiles = 0.75,
                                vector_out = TRUE),
  sd_price_elast = sd(V1),
  N = .N),
  keyby = .(fuel, saving_perc_quintile)]


# Financial wellbeing -----------------------------------------------------

colc_survey <- readRDS("./data/survey/COL_survey_processed.rds")

fw <- indiv_elast_winter[colc_survey[, .(PUPRN, E1)], on = "PUPRN"]
fw <- fw[!is.na(saving_perc_quintile)]

fw[E1 %in% c("Living comfortably",
                   "Doing alright"), combo_financial_wellbeing := "High"]
fw[E1 %in% c("Finding it very difficult",
                   "Finding it quite difficult",
                   "Just about getting by"), combo_financial_wellbeing := "Low"]
fw[, E1 := factor(E1, levels = c("Living comfortably", 
                                "Doing alright",
                                "Just about getting by",
                                "Finding it quite difficult",
                                "Finding it very difficult",
                                "Don't know",
                                "Prefer not to say",
                                "No response"))]

fw[, round(mean(V1), 3), keyby = .(fuel,E1)]
fw[, round(median(V1), 3), keyby = E1]
fw[, round(mean(V1), 3), keyby = combo_financial_wellbeing]
fw[, round(median(V1), 3), keyby = combo_financial_wellbeing]

finance_wellbeing <- fw[E1 %in% c("Living comfortably", 
                                  "Doing alright",
                                  "Just about getting by",
                                  "Finding it quite difficult",
                                  "Finding it very difficult"), 
                        .(median_elasticity = median(V1),
                          low_iqr = calc_sdc_percentiles(V1,
                                                         percentiles = 0.25,
                                                         vector_out = TRUE),
                          up_iqr = calc_sdc_percentiles(V1,
                                                        percentiles = 0.75,
                                                        vector_out = TRUE),
                          N = .N),
                        keyby = .(fuel, E1)]


# Save --------------------------------------------------------------------

fwrite(avg_winter_price_elast,
       "./data/outputs/rq7/rq7_avg_winter_price_elast.csv")

fwrite(price_elast_by_month,
       "./data/outputs/rq7/rq7_avg_monthly_price_elast.csv")

fwrite(avg_winter_price_elast_by_quintile,
       "./data/outputs/rq7/rq7_avg_quintile_price_elast.csv")

fwrite(finance_wellbeing,
       "./data/outputs/rq7/rq7_avg_financial_wellbeing_price_elast.csv")
