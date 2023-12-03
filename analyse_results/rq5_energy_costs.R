# RQ5 Bill Impacts
# (Research question 2 in the paper)


# setup -------------------------------------------------------------------

source("./scripts/setup_colc.R")
source("./scripts/functions/calc_sdc_percentiles.R")

library(lubridate)

cf <- readRDS("./data/model_training_outputs/all_counterfactuals/cf_with_costs.RDS")
training <- readRDS(paste0("./data/processed/", data_creation_date, "/all_training_with_costs.RDS"))

head(cf)
head(training)

# filter on those in both samples? (Dates back to before we required both fuels)
elec_id <- training[fuel == "elec", unique(PUPRN)] 
gas_id <- training[fuel == "gas", unique(PUPRN)] 
dual_id <- intersect(elec_id, gas_id) 

# data prep ---------------------------------------------------------------

cf <- cf[PUPRN %in% dual_id]
training <- training[PUPRN %in% dual_id]

cf[, days_in_month := days_in_month(Read_date_effective_local)]
training[, days_in_month := days_in_month(Read_date_effective_local)]


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
# For other scripts
mean_monthly_winter_costs <- this_winter_monthly_indiv[, .(mean_pounds_per_month = mean(value)), 
                                                       keyby = .(fuel, PUPRN, variable)]

# Prepare for monthly plots -----------------------------------------------

monthly_2021_22 <- prev_winter_monthly_indiv[, .(mean_monthly_pounds = mean(total_monthly_cost_pounds),
                                                 percentile_5_kWh = calc_sdc_percentiles(total_monthly_cost_pounds, 
                                                                                         0.05, 
                                                                                         TRUE),
                                                 percentile_10_kWh = calc_sdc_percentiles(total_monthly_cost_pounds, 
                                                                                          0.1, 
                                                                                          TRUE),
                                                 lower_quartile_kWh = calc_sdc_percentiles(total_monthly_cost_pounds, 
                                                                                           0.25, 
                                                                                           TRUE),
                                                 median = calc_sdc_percentiles(total_monthly_cost_pounds, 
                                                                               0.5, 
                                                                               TRUE),
                                                 upper_quartile_kWh = calc_sdc_percentiles(total_monthly_cost_pounds, 
                                                                                           0.75, 
                                                                                           TRUE),
                                                 percentile_90_kWh = calc_sdc_percentiles(total_monthly_cost_pounds, 
                                                                                          0.9, 
                                                                                          TRUE),
                                                 percentile_95_kWh = calc_sdc_percentiles(total_monthly_cost_pounds, 
                                                                                          0.95, 
                                                                                          TRUE)), 
                                             keyby = .(fuel, month)]
monthly_2021_22[, variable := "observed_energy_kWh_2021_22"]

monthly_2022_23 <- this_winter_monthly_indiv[, .(mean_monthly_pounds = mean(value),
                                                 percentile_5_kWh = calc_sdc_percentiles(value, 
                                                                                         0.05, 
                                                                                         TRUE),
                                                 percentile_10_kWh = calc_sdc_percentiles(value, 
                                                                                          0.1, 
                                                                                          TRUE),
                                                 lower_quartile_kWh = calc_sdc_percentiles(value, 
                                                                                           0.25, 
                                                                                           TRUE),
                                                 median = calc_sdc_percentiles(value, 
                                                                               0.5, 
                                                                               TRUE),
                                                 upper_quartile_kWh = calc_sdc_percentiles(value, 
                                                                                           0.75, 
                                                                                           TRUE),
                                                 percentile_90_kWh = calc_sdc_percentiles(value, 
                                                                                          0.9, 
                                                                                          TRUE),
                                                 percentile_95_kWh = calc_sdc_percentiles(value, 
                                                                                          0.95, 
                                                                                          TRUE)), 
                                             keyby = .(fuel, month, variable)]

monthly_2022_23[, variable := paste0(variable, "_2022_23")]

all_monthly <- rbind(monthly_2021_22, monthly_2022_23)

all_monthly[, N_households := ifelse(fuel == "elec", 
                                     this_winter_monthly_indiv[fuel == "elec", length(unique(PUPRN))],
                                     this_winter_monthly_indiv[fuel == "gas", length(unique(PUPRN))])]

# monthly plots -----------------------------------------------------------

# Bar chart
ggplot(all_monthly[!variable %in% c("savings_kWh_2022_23",
                                    "savings_perc_2022_23")],
       aes(x = factor(month, levels = c(10:12, 1:3)),
           y = mean_monthly_pounds,
           fill = variable)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_wrap(~fuel)

ggplot(all_monthly[variable %in% c("savings_kWh_2022_23",
                                    "savings_perc_2022_23")],
       aes(x = factor(month, levels = c(10:12, 1:3)),
           y = mean_monthly_pounds)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(variable~fuel)

# Box plot
ggplot(all_monthly[!variable %in% c("savings_kWh_2022_23",
                                    "savings_perc_2022_23")]) + 
  geom_boxplot(aes(x = factor(month, levels = c(10:12, 1:3)),
                   ymin = percentile_5_kWh,
                   lower = lower_quartile_kWh,
                   middle = median,
                   upper = upper_quartile_kWh,
                   ymax = percentile_95_kWh,
                   fill = factor(variable, 
                                 levels = c("observed_energy_kWh_2021_22",
                                            "predicted_energy_kWh_2022_23",
                                            "observed_energy_kWh_2022_23"))),
               stat = "identity") + 
  facet_wrap(~fuel) + 
  theme(legend.title = element_blank())


# Savings £
ggplot(all_monthly[variable == "savings_kWh_2022_23"]) + 
  geom_boxplot(aes(x = factor(month, levels = c(10:12, 1:3)),
                   ymin = percentile_5_kWh,
                   lower = lower_quartile_kWh,
                   middle = median,
                   upper = upper_quartile_kWh,
                   ymax = percentile_95_kWh),
               stat = "identity") + 
  facet_wrap(~fuel) + 
  theme(legend.title = element_blank())

ggplot(all_monthly[variable == "savings_perc_2022_23"]) + 
  geom_boxplot(aes(x = factor(month, levels = c(10:12, 1:3)),
                   ymin = percentile_5_kWh,
                   lower = lower_quartile_kWh,
                   middle = median,
                   upper = upper_quartile_kWh,
                   ymax = percentile_95_kWh),
               stat = "identity") + 
  facet_wrap(~fuel) + 
  theme(legend.title = element_blank())




# Whole winter table for output -------------------------------------------

winter_2021_22 <- prev_winter_monthly_indiv[, .(mean_monthly_pounds = mean(total_monthly_cost_pounds),
                                                 percentile_5_kWh = calc_sdc_percentiles(total_monthly_cost_pounds, 
                                                                                         0.05, 
                                                                                         TRUE),
                                                 lower_quartile_kWh = calc_sdc_percentiles(total_monthly_cost_pounds, 
                                                                                           0.25, 
                                                                                           TRUE),
                                                 median = calc_sdc_percentiles(total_monthly_cost_pounds, 
                                                                               0.5, 
                                                                               TRUE),
                                                 upper_quartile_kWh = calc_sdc_percentiles(total_monthly_cost_pounds, 
                                                                                           0.75, 
                                                                                           TRUE),
                                                 percentile_95_kWh = calc_sdc_percentiles(total_monthly_cost_pounds, 
                                                                                          0.95, 
                                                                                          TRUE),
                                                sd = sd(total_monthly_cost_pounds),
                                                N = .N), 
                                             keyby = .(fuel)]
winter_2021_22[, variable := "observed_energy_kWh_2021_22"]

winter_2022_23 <- this_winter_monthly_indiv[, .(mean_monthly_pounds = mean(value),
                                                 percentile_5_kWh = calc_sdc_percentiles(value, 
                                                                                         0.05, 
                                                                                         TRUE),
                                                 lower_quartile_kWh = calc_sdc_percentiles(value, 
                                                                                           0.25, 
                                                                                           TRUE),
                                                 median = calc_sdc_percentiles(value, 
                                                                               0.5, 
                                                                               TRUE),
                                                 upper_quartile_kWh = calc_sdc_percentiles(value, 
                                                                                           0.75, 
                                                                                           TRUE),
                                                 percentile_95_kWh = calc_sdc_percentiles(value, 
                                                                                          0.95, 
                                                                                          TRUE),
                                            sd = sd(value),
                                            N = .N), 
                                            keyby = .(fuel, variable)]

winter_2022_23[, variable := paste0(variable, "_2022_23")]

all_winter_stats <- rbind(winter_2021_22, winter_2022_23)
setcolorder(all_winter_stats,
            c("fuel", "variable"))
setkey(all_winter_stats, "fuel")
all_winter_stats[, N_households := length(dual_id)]


# Price stats -------------------------------------------------------------

indiv_prices <- unique(cf, by = c("PUPRN", "period", "fuel"))
indiv_prev_prices <- unique(training, by = c("PUPRN", "period", "fuel"))

price_stats <- indiv_prices[, .(mean_standing_charge = mean(standing_charge),
                      min_standing_charge = min(standing_charge),
                      max_standing_charge = max(standing_charge),
                      mean_unit_cost = mean(unit_cost),
                      min_unit_cost = min(unit_cost),
                      max_unit_cost = max(unit_cost),
                      N = .N),
                  keyby = .(fuel, period)]

price_stats_prev <- indiv_prev_prices[, .(mean_standing_charge = mean(standing_charge),
                                          min_standing_charge = min(standing_charge),
                                          max_standing_charge = max(standing_charge),
                                          mean_unit_cost = mean(unit_cost),
                                          min_unit_cost = min(unit_cost),
                                          max_unit_cost = max(unit_cost),
                                          N = .N),
                                      keyby = .(fuel, period)]

price_stats <- rbind(price_stats_prev,
                     price_stats)
price_stats[, period := factor(period,
                               levels = c("Oct21_Mar22",
                                          "Oct_Dec_22",
                                          "Jan_Mar_23"))]
setkeyv(price_stats, c("fuel", "period"))

# Save --------------------------------------------------------------------


fwrite(all_monthly,
       paste0("S:/SERL_Frontier1/EZW/data/outputs/rq5/monthly_total_costs.csv"))
fwrite(all_winter_stats,
       paste0("S:/SERL_Frontier1/EZW/data/outputs/rq5/winter_cost_stats.csv"))
fwrite(price_stats, 
       "./data/outputs/rq5/rq5_price_stats.csv")


saveRDS(mean_monthly_winter_costs,
        "./data/analysis_data/intermediary/mean_monthly_winter_costs.RDS")
