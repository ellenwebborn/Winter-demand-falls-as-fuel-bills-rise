# Research question 1: Overall sample-level impact of the cost-of-living crisis


# Setup -------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(broom)
library(purrr)

source("./scripts/functions/calc_sdc_percentiles.R")

long_energy <- readRDS("./data/model_training_outputs/all_counterfactuals/all_cf_long.RDS")


# Household-level stats ---------------------------------------------------

# means for each household
winter_mean <- long_energy[, .(mean_kWh = mean(value, na.rm = TRUE)),
                           keyby = .(fuel, PUPRN, variable)]

monthly_mean <- long_energy[, .(mean_kWh = mean(value, na.rm = TRUE),
                                mean_min_temp_C = mean(min_2m_temp_C),
                                mean_mean_temp_C = mean(mean_2m_temp_C),
                                mean_max_temp_C = mean(max_2m_temp_C)),
                            keyby = .(fuel, PUPRN, variable, month)]

weekly_mean <- long_energy[, .(mean_kWh = mean(value, na.rm = TRUE)),
                            keyby = .(fuel, PUPRN, variable, week)]


# Add percentage changes --------------------------------------------------

# Overall
wide_winter_mean <- dcast(winter_mean, fuel + PUPRN ~ variable, value.var = "mean_kWh")
wide_winter_mean[, savings_perc := savings_kWh / predicted_energy_kWh * 100]
winter_mean <- melt(wide_winter_mean, 
                    id.vars = c("fuel", "PUPRN"),
                    measure.vars = c("observed_energy_kWh",
                                     "predicted_energy_kWh",
                                     "savings_kWh",
                                     "savings_perc"))

# Monthly
wide_monthly_mean <- dcast(monthly_mean, fuel + PUPRN + month + 
                             mean_min_temp_C + mean_mean_temp_C + mean_max_temp_C ~ variable, 
                           value.var = "mean_kWh")
wide_monthly_mean[, savings_perc := savings_kWh / predicted_energy_kWh * 100]
wide_monthly_mean[observed_energy_kWh == 0 & predicted_energy_kWh == 0, 
                  savings_perc := 0]
monthly_mean <- melt(wide_monthly_mean, 
                    id.vars = c("fuel", "PUPRN", "month", "mean_min_temp_C",
                                "mean_mean_temp_C", "mean_max_temp_C"),
                    measure.vars = c("observed_energy_kWh",
                                     "predicted_energy_kWh",
                                     "savings_kWh",
                                     "savings_perc"))

# remove infinite values, due to inf perc changes where all predictions are 0
monthly_mean <- monthly_mean[!is.infinite(value)]


# Weekly
wide_weekly_mean <- dcast(weekly_mean, fuel + PUPRN + week ~ variable, 
                           value.var = "mean_kWh")
wide_weekly_mean[, savings_perc := savings_kWh / predicted_energy_kWh * 100]
wide_weekly_mean[observed_energy_kWh == 0 & predicted_energy_kWh == 0, 
                 savings_perc := 0]

weekly_mean <- melt(wide_weekly_mean, 
                     id.vars = c("fuel", "PUPRN", "week"),
                     measure.vars = c("observed_energy_kWh",
                                      "predicted_energy_kWh",
                                      "savings_kWh",
                                      "savings_perc"))
weekly_mean <- weekly_mean[!is.infinite(value)]


# Add quintile information ------------------------------------------------

# Total consumption quintile
winter_mean_total_fuel <- dcast(winter_mean,
                                PUPRN + variable ~ fuel,
                                value.var = "value")

winter_mean_total_fuel[, total := elec + gas]
total_consumption_quintile_bounds <- winter_mean_total_fuel[variable == "observed_energy_kWh", .(quintile = 0:5,
                                                              upper_lim = quantile(total, 
                                                                                  seq(0, 1, length = 6)))]
total_consumption_quintile_info <- winter_mean_total_fuel[variable == "observed_energy_kWh"]
total_consumption_quintile_info[, tot_obs_quintile := findInterval(total, total_consumption_quintile_bounds$upper_lim)]
total_consumption_quintile_info[tot_obs_quintile == 6, tot_obs_quintile := 5]

total_consumption_quintile_stats <- total_consumption_quintile_info[, .(mean_elec_kWh = mean(elec),
                                                                        mean_gas_kWh = mean(gas),
                                                                        mean_tot_kWh = mean(total),
                                                                        median_elec_kWh = median(elec),
                                                                        median_gas_kWh = median(gas),
                                                                        median_tot_kWh = median(total),
                                                                        lower_iqr_elec = calc_sdc_percentiles(elec, 0.25, TRUE),
                                                                        lower_iqr_gas = calc_sdc_percentiles(gas, 0.25, TRUE),
                                                                        lower_iqr_tot = calc_sdc_percentiles(total, 0.25, TRUE),
                                                                        upper_iqr_elec = calc_sdc_percentiles(elec, 0.75, TRUE),
                                                                        upper_iqr_gas = calc_sdc_percentiles(gas, 0.75, TRUE),
                                                                        upper_iqr_tot = calc_sdc_percentiles(total, 0.75, TRUE),
                                                                        sd_elec = sd(elec),
                                                                        sd_gas = sd(gas),
                                                                        sd_tot = sd(total),
                                                                        N = .N),
                                                                    keyby = tot_obs_quintile]
total_consumption_quintile_stats

# Add information to key tables for future use
winter_mean <- total_consumption_quintile_info[, .(PUPRN, tot_obs_quintile)][winter_mean, on = "PUPRN"]
long_energy <- total_consumption_quintile_info[, .(PUPRN, tot_obs_quintile)][long_energy, on = "PUPRN"]



# Percentage saving quintile
tot_saving_percent <- dcast(winter_mean_total_fuel[variable %in% c("observed_energy_kWh",
                                                                   "predicted_energy_kWh"), 
                                                   .(PUPRN, variable, total)],
                            PUPRN ~ variable,
                            value.var = "total")
tot_saving_percent[, tot_saving_perc := (predicted_energy_kWh - observed_energy_kWh) / predicted_energy_kWh * 100]

perc_saving_quintile_bounds <- tot_saving_percent[, .(quintile = 0:5, 
                                                      upper_lim = quantile(tot_saving_perc, 
                                                                           seq(0, 1, length = 6)))]
perc_saving_quintile_info <- tot_saving_percent
perc_saving_quintile_info[, saving_perc_quintile := findInterval(tot_saving_perc, 
                                                                 perc_saving_quintile_bounds$upper_lim)]
perc_saving_quintile_info[, .N, keyby = saving_perc_quintile]
perc_saving_quintile_info[saving_perc_quintile == 6, saving_perc_quintile := 5]

# Replace the percentage change of total energy use with the correctly calculated value
winter_mean_total_fuel <- perc_saving_quintile_info[, .(PUPRN, 
                                                        tot_saving_perc,
                                                        saving_perc_quintile)][winter_mean_total_fuel,
                                                                                      on = "PUPRN"]
winter_mean_total_fuel[variable == "savings_perc", total := tot_saving_perc]
winter_mean_total_fuel[, tot_saving_perc := NULL]

perc_saving_quintile_stats <-
  winter_mean_total_fuel[, .(
    mean_elec_kWh = mean(elec),
    mean_gas_kWh = mean(gas),
    mean_tot_kWh = mean(total),
    median_elec_kWh = median(elec),
    median_gas_kWh = median(gas),
    median_tot_kWh = median(total),
    lower_iqr_elec = calc_sdc_percentiles(elec, 0.25, TRUE),
    lower_iqr_gas = calc_sdc_percentiles(gas, 0.25, TRUE),
    lower_iqr_tot = calc_sdc_percentiles(total, 0.25, TRUE),
    upper_iqr_elec = calc_sdc_percentiles(elec, 0.75, TRUE),
    upper_iqr_gas = calc_sdc_percentiles(gas, 0.75, TRUE),
    upper_iqr_tot = calc_sdc_percentiles(total, 0.75, TRUE),
    sd_elec = sd(elec),
    sd_gas = sd(gas),
    sd_tot = sd(total),
    N = .N),
  keyby = .(variable, saving_perc_quintile)]
perc_saving_quintile_stats

perc_saving_quintile_5_vs_rest_stats <- 
  winter_mean_total_fuel[, .(
    mean_elec_kWh = mean(elec),
    mean_gas_kWh = mean(gas),
    mean_tot_kWh = mean(total),
    median_elec_kWh = median(elec),
    median_gas_kWh = median(gas),
    median_tot_kWh = median(total),
    lower_iqr_elec = calc_sdc_percentiles(elec, 0.25, TRUE),
    lower_iqr_gas = calc_sdc_percentiles(gas, 0.25, TRUE),
    lower_iqr_tot = calc_sdc_percentiles(total, 0.25, TRUE),
    upper_iqr_elec = calc_sdc_percentiles(elec, 0.75, TRUE),
    upper_iqr_gas = calc_sdc_percentiles(gas, 0.75, TRUE),
    upper_iqr_tot = calc_sdc_percentiles(total, 0.75, TRUE),
    sd_elec = sd(elec),
    sd_gas = sd(gas),
    sd_tot = sd(total),
    N = .N),
    keyby = .(variable, saving_perc_quintile == 5)]

perc_saving_quintile_5_vs_rest_stats[, saving_perc_quintile := as.integer(saving_perc_quintile)]
perc_saving_quintile_5_vs_rest_stats[, saving_perc_quintile := ifelse(saving_perc_quintile == 1, 5, 14)]
perc_saving_quintile_5_vs_rest_stats

perc_saving_quintile_stats <- rbind(perc_saving_quintile_stats,
                                    perc_saving_quintile_5_vs_rest_stats[saving_perc_quintile != 5])

# Add information to key tables for future use
winter_mean <- perc_saving_quintile_info[, .(PUPRN, saving_perc_quintile)][winter_mean, on = "PUPRN"]
long_energy <- perc_saving_quintile_info[, .(PUPRN, saving_perc_quintile)][long_energy, on = "PUPRN"]


# quintile bounds

sdc_bounds <- calc_sdc_percentiles(tot_saving_percent$tot_saving_perc, 
                     percentiles = seq(0,1, by = 0.2))

# Stats -------------------------------------------------------------------


# sample mean, IQR, SD, N
sample_means <- winter_mean[, .(mean_kWh = mean(value, na.rm = TRUE),
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
                                                                        TRUE),
                                sd = sd(value),
                                N = .N),
                            keyby = .(fuel, variable)]

# monthly sample mean, IQR, SD, N
monthly_sample_means <- monthly_mean[,  .(mean_min_temp_C = mean(mean_min_temp_C),
                                          mean_mean_temp_C = mean(mean_mean_temp_C),
                                          mean_max_temp_C = mean(mean_max_temp_C),
                                          mean_kWh = mean(value, na.rm = TRUE),
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
                                                                                   TRUE),
                                        sd = sd(value),
                                        N = .N),
                                    keyby = .(fuel, variable, month)]


# weekly sample mean, IQR, SD, N
weekly_sample_means <- weekly_mean[,  .(mean_kWh = mean(value, na.rm = TRUE),
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
                                                                                 TRUE),
                                         sd = sd(value),
                                         N = .N),
                                     keyby = .(fuel, variable, week)]

# daily sample mean, IQR, SD, N
daily_sample_means <- long_energy[, .(mean_kWh = mean(value, na.rm = TRUE),
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
                                                                               TRUE),
                                       sd = sd(value),
                                       N = .N),
                                   keyby = .(fuel, variable, Read_date_effective_local)]





# Hypothesis testing ------------------------------------------------------

# H1: elec and gas consumption observations lower than counterfactuals (1-sided)
#  testing if mean of the differences is different to zero


# H1a Elec
elec_t_test <- t.test(winter_mean[fuel == "elec" & variable == "savings_kWh", value],
                      mu = 0, alternative = "greater") 
# p-value < 2.2e-16, 95% CI (0.988, inf), t = 26.205
elec_t_test$p.value
elec_t_test$statistic

# H1a Gas
gas_t_test <- t.test(winter_mean[fuel == "gas" & variable == "savings_kWh", value],
                     mu = 0, alternative = "greater") 
# p-value < 2.2e-16, 95% CI (7.582, inf), t = 54.767
gas_t_test

t_test_results <- cbind(data.table(fuel = c("elec", "gas")),
                        as.data.table(map_df(list(elec_t_test, gas_t_test), tidy)))



# Elec Histogram data ----------------------------------------------------------

# understand data limits
winter_mean[variable %in% c("observed_energy_kWh",
                            "predicted_energy_kWh"), 
            quantile(value, 0.95), keyby = .(fuel, variable)]

winter_mean[variable %in% c("observed_energy_kWh",
                            "predicted_energy_kWh"), 
            quantile(value, 0.98), keyby = .(fuel, variable)]

winter_mean[variable %in% c("observed_energy_kWh",
                            "predicted_energy_kWh"), 
            quantile(value, 0.99), keyby = .(fuel, variable)]



# Electricity observations and predictions
winter_mean2 <- copy(winter_mean)
max_obs_pred_value <- 30
bin_width = 1.3
elec_obs_pred_breaks <- seq(0, max_obs_pred_value, by = bin_width)
winter_mean2[variable %in% c("observed_energy_kWh",
                             "predicted_energy_kWh") & 
               fuel == "elec" & 
               value < max_obs_pred_value, 
             bin := findInterval(value, elec_obs_pred_breaks)]

counts_elec_obs_preds <- winter_mean2[variable %in% c("observed_energy_kWh",
                                                       "predicted_energy_kWh") & 
                                         fuel == "elec" & 
                                        !is.na(bin), 
                                      .(N = .N,
                                        xmin = (bin - 1) * bin_width,
                                        xmax =  bin * bin_width,
                                        x_mid = bin * bin_width - bin_width / 2), 
                                       keyby = .(fuel, variable, bin)]

elec_obs_pred_hist <- ggplot(counts_elec_obs_preds) + 
                             geom_histogram(stat = "identity",
                                              position = "dodge", 
                                              aes(x = x_mid,
                                                  y = N,
                                                  fill = variable,
                                                  alpha = 0.2)) 
elec_obs_pred_hist
counts_elec_obs_preds
counts_elec_obs_preds[, min(N)]



# Electricity savings
# understand data limits
winter_mean[variable == "savings_kWh", 
            quantile(value, 0.95), keyby = .(fuel, variable)]
winter_mean[variable == "savings_kWh", 
            quantile(value, 0.05), keyby = .(fuel, variable)]
winter_mean[variable == "savings_kWh", 
            quantile(value, 0.99), keyby = .(fuel, variable)]
winter_mean[variable == "savings_kWh", 
            quantile(value, 0.01), keyby = .(fuel, variable)]

winter_mean2 <- copy(winter_mean)
max_value <- 9
min_value <- -5
bin_width <- 0.5
elec_savings_breaks <- seq(min_value, max_value, by = bin_width)
winter_mean2[variable == "savings_kWh" & 
               fuel == "elec" & 
               value < max_value & value >= min_value, 
             bin := findInterval(value, elec_savings_breaks)]

counts_elec_savings <- winter_mean2[variable == "savings_kWh" & 
                                        fuel == "elec" & 
                                        !is.na(bin), 
                                      .(N = .N,
                                        xmin = min_value + (bin - 1) * bin_width,
                                        xmax =  min_value + bin * bin_width,
                                        x_mid = min_value + bin * bin_width - bin_width / 2), 
                                      keyby = .(fuel, variable, bin)]

elec_savings_hist <- ggplot(counts_elec_savings) + 
  geom_histogram(stat = "identity",
                 position = "identity", 
                 aes(x = x_mid,
                     y = N,
                     fill = variable,
                     alpha = 0.2)) 
elec_savings_hist
counts_elec_savings
counts_elec_savings[, min(N)]


# Electricity Percentage savings
# understand data limits
winter_mean[variable == "savings_perc", 
            quantile(value, 0.95), keyby = .(fuel, variable)]
winter_mean[variable == "savings_perc", 
            quantile(value, 0.05), keyby = .(fuel, variable)]
winter_mean[variable == "savings_perc", 
            quantile(value, 0.99), keyby = .(fuel, variable)]
winter_mean[variable == "savings_perc", 
            quantile(value, 0.01), keyby = .(fuel, variable)]

winter_mean2 <- copy(winter_mean)
max_value <- 60
min_value <- -45
bin_width = 3.5
elec_savings_breaks <- seq(min_value, max_value, by = bin_width)
winter_mean2[variable == "savings_perc" & 
               fuel == "elec" & 
               value < max_value & value >= min_value, 
             bin := findInterval(value, elec_savings_breaks)]

counts_elec_perc_savings <- winter_mean2[variable == "savings_perc" & 
                                      fuel == "elec" & 
                                      !is.na(bin), 
                                    .(N = .N,
                                      xmin = min_value + (bin - 1) * bin_width,
                                      xmax =  min_value + bin * bin_width,
                                      x_mid = min_value + bin * bin_width - bin_width / 2), 
                                    keyby = .(fuel, variable, bin)]

elec_savings_perc_hist <- ggplot(counts_elec_perc_savings) + 
  geom_histogram(stat = "identity",
                 position = "identity", 
                 aes(x = x_mid,
                     y = N,
                     fill = variable,
                     alpha = 0.2)) 
elec_savings_perc_hist
counts_elec_perc_savings
counts_elec_perc_savings[, min(N)]


# Gas Histogram data ----------------------------------------------------------

# understand data limits
winter_mean[variable %in% c("observed_energy_kWh",
                            "predicted_energy_kWh"), 
            quantile(value, 0.95), keyby = .(fuel, variable)]

winter_mean[variable %in% c("observed_energy_kWh",
                            "predicted_energy_kWh"), 
            quantile(value, 0.98), keyby = .(fuel, variable)]

winter_mean[variable %in% c("observed_energy_kWh",
                            "predicted_energy_kWh"), 
            quantile(value, 0.99), keyby = .(fuel, variable)]



# Gas observations and predictions
winter_mean2 <- copy(winter_mean)
max_obs_pred_value <- 150
bin_width = 6
gas_obs_pred_breaks <- seq(0, max_obs_pred_value, by = bin_width)
winter_mean2[variable %in% c("observed_energy_kWh",
                             "predicted_energy_kWh") & 
               fuel == "gas" & 
               value < max_obs_pred_value, 
             bin := findInterval(value, gas_obs_pred_breaks)]

counts_gas_obs_preds <- winter_mean2[variable %in% c("observed_energy_kWh",
                                                      "predicted_energy_kWh") & 
                                        fuel == "gas" & 
                                        !is.na(bin), 
                                      .(N = .N,
                                        xmin = (bin - 1) * bin_width,
                                        xmax =  bin * bin_width,
                                        x_mid = bin * bin_width - bin_width / 2), 
                                      keyby = .(fuel, variable, bin)]

gas_obs_pred_hist <- ggplot(counts_gas_obs_preds) + 
  geom_histogram(stat = "identity",
                 position = "dodge", 
                 aes(x = x_mid,
                     y = N,
                     fill = variable,
                     alpha = 0.2)) 
gas_obs_pred_hist
counts_gas_obs_preds
counts_gas_obs_preds[, min(N)]


# Gas savings
# understand data limits
winter_mean[variable == "savings_kWh", 
            quantile(value, 0.95), keyby = .(fuel, variable)]
winter_mean[variable == "savings_kWh", 
            quantile(value, 0.05), keyby = .(fuel, variable)]
winter_mean[variable == "savings_kWh", 
            quantile(value, 0.99), keyby = .(fuel, variable)]
winter_mean[variable == "savings_kWh", 
            quantile(value, 0.01), keyby = .(fuel, variable)]

winter_mean2 <- copy(winter_mean)
max_value <- 40
min_value <- -20
bin_width = 2
gas_savings_breaks <- seq(min_value, max_value, by = bin_width)
winter_mean2[variable == "savings_kWh" & 
               fuel == "gas" & 
               value < max_value & value >= min_value, 
             bin := findInterval(value, gas_savings_breaks)]

counts_gas_savings <- winter_mean2[variable == "savings_kWh" & 
                                      fuel == "gas" & 
                                      !is.na(bin), 
                                    .(N = .N,
                                      xmin = min_value + (bin - 1) * bin_width,
                                      xmax =  min_value + bin * bin_width,
                                      x_mid = min_value + bin * bin_width - bin_width / 2), 
                                    keyby = .(fuel, variable, bin)]

gas_savings_hist <- ggplot(counts_gas_savings) + 
  geom_histogram(stat = "identity",
                 position = "identity", 
                 aes(x = x_mid,
                     y = N,
                     fill = variable,
                     alpha = 0.2)) 
gas_savings_hist
counts_gas_savings
counts_gas_savings[, min(N)]



# Gas Percentage savings
# understand data limits
winter_mean[variable == "savings_perc", 
            quantile(value, 0.95), keyby = .(fuel, variable)]
winter_mean[variable == "savings_perc", 
            quantile(value, 0.05), keyby = .(fuel, variable)]
winter_mean[variable == "savings_perc", 
            quantile(value, 0.99), keyby = .(fuel, variable)]
winter_mean[variable == "savings_perc", 
            quantile(value, 0.01), keyby = .(fuel, variable)]

winter_mean2 <- copy(winter_mean)
max_value <- 75
min_value <- -40
bin_width = 4
gas_savings_breaks <- seq(min_value, max_value, by = bin_width)
winter_mean2[variable == "savings_perc" & 
               fuel == "gas" & 
               value < max_value & value >= min_value, 
             bin := findInterval(value, gas_savings_breaks)]

counts_gas_perc_savings <- winter_mean2[variable == "savings_perc" & 
                                           fuel == "gas" & 
                                           !is.na(bin), 
                                         .(N = .N,
                                           xmin = min_value + (bin - 1) * bin_width,
                                           xmax =  min_value + bin * bin_width,
                                           x_mid = min_value + bin * bin_width - bin_width / 2), 
                                         keyby = .(fuel, variable, bin)]

gas_savings_perc_hist <- ggplot(counts_gas_perc_savings) + 
  geom_histogram(stat = "identity",
                 position = "identity", 
                 aes(x = x_mid,
                     y = N,
                     fill = variable,
                     alpha = 0.2)) 
gas_savings_perc_hist
counts_gas_perc_savings
counts_gas_perc_savings[, min(N)]


# Combine histogram data --------------------------------------------------

hist_data <- rbind(counts_elec_obs_preds,
                   counts_elec_savings,
                   counts_elec_perc_savings,
                   counts_gas_obs_preds,
                   counts_gas_savings,
                   counts_gas_perc_savings)

hist_data[, min(N)] # 10


# Export ------------------------------------------------------------------

# write them individually to csv
fwrite(sample_means,
       paste0("./data/outputs/rq1/winter_sample_means.csv"))
fwrite(monthly_sample_means,
       paste0("./data/outputs/rq1/monthly_sample_means.csv"))
fwrite(weekly_sample_means,
       paste0("./data/outputs/rq1/weekly_sample_means.csv"))
fwrite(daily_sample_means,
       paste0("./data/outputs/rq1/daily_sample_means.csv"))
fwrite(t_test_results,
       paste0("./data/outputs/rq1/t_test_results.csv"))
fwrite(hist_data,
       paste0("./data/outputs/rq1/rq1_histogram_data.csv"))
fwrite(perc_saving_quintile_stats,
       paste0("./data/outputs/rq6/rq6_perc_saving_quintile_stats.csv"))
fwrite(sdc_bounds,
       "./data/outputs/rq6/rq6_sdc_safe_quintile_bounds.csv")

# use in other scripts
saveRDS(winter_mean, "./data/analysis_data/intermediary/individual_winter_savings.RDS")
saveRDS(long_energy, "./data/model_training_outputs/all_counterfactuals/all_cf_long_with_quintiles.RDS")
saveRDS(winter_mean_total_fuel, "./data/model_training_outputs/all_counterfactuals/winter_mean_total_fuel.RDS")
