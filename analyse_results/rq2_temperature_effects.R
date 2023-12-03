# Research question 2: Savings versus temperature
# Not a research question in the paper - section 3.5 analysis. 



# Setup -------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(broom)
library(purrr)

source("./scripts/functions/calc_sdc_percentiles.R")

long_energy <- readRDS("./data/model_training_outputs/all_counterfactuals/all_cf_long_with_quintiles.RDS")


# Bin by temperature interval -----------------------------------------------

long_energy[, min(mean_2m_temp_C)] 
long_energy[, max(mean_2m_temp_C)]

#bin by temperature
min_temp_break <- -20
max_temp_break <- 20
bin_width <- 1

min_temp_to_plot <- 0# previously -5
max_temp_to_plot <- 15

temp_breaks <- seq(min_temp_break, max_temp_break, by = bin_width)
long_energy[, bin := findInterval(mean_2m_temp_C, temp_breaks)]



# Filter households with too much data out of temp range ------------------

### see how many have a point in every included bin

long_energy_temp_range <- long_energy[mean_2m_temp_C >= min_temp_to_plot & 
                                        mean_2m_temp_C <= max_temp_to_plot]
indiv_counts_per_bin <- long_energy_temp_range[variable == "observed_energy_kWh", 
                                               .N, keyby = .(fuel, PUPRN, bin)]

indiv_n_bins <- indiv_counts_per_bin[, .N, keyby = .(fuel, PUPRN)]
indiv_n_bins
n_included_bins <- long_energy_temp_range[, length(unique(bin))] # 15 bins
indiv_n_bins[, .N, keyby = .(fuel, N)]

# Filter
long_energy_temp_range <- indiv_n_bins[long_energy_temp_range, on = .(fuel, PUPRN)]

median_long_energy_temp_range <- long_energy_temp_range[N == n_included_bins, 
                                                        .(value = median(value),
                                                          mean_2m_temp_C = mean(mean_2m_temp_C)),
                                                        keyby = .(fuel, PUPRN, variable, bin, N)]

median_energy_by_temp_bin <- median_long_energy_temp_range[, 
                                       .(mean_temp = mean(mean_2m_temp_C),
                                         mean_energy = mean(value, na.rm = TRUE),
                                         median_energy = calc_sdc_percentiles(value, 0.5)$approx,
                                         low_iqr = calc_sdc_percentiles(value, 0.25)$approx,
                                         high_iqr = calc_sdc_percentiles(value, 0.75)$approx,
                                         N = .N,
                                         N_PUPRN = length(unique(PUPRN))),
                                       keyby = .(fuel, variable, bin)]


ggplot(median_energy_by_temp_bin,
       aes(x = mean_temp,
           y = mean_energy,
           colour = factor(variable))) + 
  geom_point() + 
  geom_line() +
  facet_wrap(~fuel)

# Fuels separately
ggplot(median_energy_by_temp_bin[fuel == "elec"],
       aes(x = mean_temp,
           y = median_energy,
           colour = factor(variable))) + 
  geom_point() + 
  geom_line() +
  ggtitle("Electricity")

ggplot(median_energy_by_temp_bin[fuel == "gas"],
       aes(x = mean_temp,
           y = median_energy,
           colour = factor(variable))) + 
  geom_point() + 
  geom_line() +
  ggtitle("Gas")


# Savings separately ------------------------------------------------------

long_savings <- melt(median_energy_by_temp_bin[variable == "savings_kWh"], 
                     id.vars = c("fuel", "bin", "mean_temp"),
                     measure.vars = c("median_energy", "low_iqr", "high_iqr"))

ggplot(long_savings[fuel == "gas" ],
       aes(x = mean_temp,
           y = value,
           colour = factor(variable))) + 
  geom_point() + 
  geom_line() +
  ggtitle("Gas")

ggplot(long_savings[fuel == "elec" ],
       aes(x = mean_temp,
           y = value,
           colour = factor(variable))) + 
  geom_point() + 
  geom_line() +
  ggtitle("Elec")




# Percentage changes ------------------------------------------------------

wide_median_energy_by_temp_bin <- dcast(median_energy_by_temp_bin,
                                      fuel + bin + mean_temp + N + N_PUPRN ~ variable,
                                      value.var = c("median_energy",
                                                    "low_iqr",
                                                    "high_iqr"))
wide_median_energy_by_temp_bin[, `:=`(perc_change_median = median_energy_savings_kWh / median_energy_predicted_energy_kWh * 100,
                                    perc_change_low_iqr = low_iqr_savings_kWh / low_iqr_predicted_energy_kWh * 100,
                                    perc_change_high_iqr = high_iqr_savings_kWh / high_iqr_predicted_energy_kWh * 100)]

long_median_energy_by_temp_bin <- melt(wide_median_energy_by_temp_bin,
                                     id.vars = c("fuel", "bin", "mean_temp", "N", "N_PUPRN"),
                                     measure.vars = c("perc_change_median",
                                                      "perc_change_low_iqr",
                                                      "perc_change_high_iqr"))

ggplot(long_median_energy_by_temp_bin[fuel == "gas" ],
       aes(x = mean_temp,
           y = value,
           colour = factor(variable))) + 
  geom_point() + 
  geom_line() +
  ggtitle("Gas")

ggplot(long_median_energy_by_temp_bin[fuel == "elec" ],
       aes(x = mean_temp,
           y = value,
           colour = factor(variable))) + 
  geom_point() + 
  geom_line() +
  ggtitle("Elec")

ggplot(long_median_energy_by_temp_bin,
       aes(x = mean_temp,
           y = value,
           colour = factor(variable))) + 
  geom_point() + 
  geom_line() +
  facet_wrap(~fuel)



# By saver quintile -----------------------------------------------------

median_long_energy_saver_quintile <- long_energy_temp_range[N == n_included_bins, 
                                                        .(value = median(value),
                                                          mean_2m_temp_C = mean(mean_2m_temp_C)),
                                                        keyby = .(fuel, PUPRN, variable, bin, 
                                                                  saving_perc_quintile)]

median_long_energy_saver_quintile_by_temp_bin <- median_long_energy_saver_quintile[, 
                                                           .(mean_temp = mean(mean_2m_temp_C),
                                                             mean_energy = mean(value, na.rm = TRUE),
                                                             median_energy = calc_sdc_percentiles(value, 0.5)$approx,
                                                             low_iqr = calc_sdc_percentiles(value, 0.25)$approx,
                                                             high_iqr = calc_sdc_percentiles(value, 0.75)$approx,
                                                             N = .N,
                                                             N_PUPRN = length(unique(PUPRN))),
                                                           keyby = .(fuel, variable, saving_perc_quintile, bin)]

# All quintiles together
ggplot(median_long_energy_saver_quintile_by_temp_bin[fuel == "gas" & variable != "savings_kWh"],
       aes(x = mean_temp,
           y = median_energy,
           colour = factor(saving_perc_quintile))) + 
  geom_point() + 
  geom_line(aes(linetype = factor(variable))) +
  ggtitle("Gas") + 
  facet_grid(.~factor(variable, levels = c("predicted_energy_kWh", "observed_energy_kWh")))


ggplot(median_long_energy_saver_quintile_by_temp_bin[fuel == "elec" & variable != "savings_kWh"],
       aes(x = mean_temp,
           y = median_energy,
           colour = factor(saving_perc_quintile))) + 
  geom_point() + 
  geom_line(aes(linetype = factor(variable))) +
  ggtitle("Electricity")  + 
  facet_grid(.~factor(variable, levels = c("predicted_energy_kWh", "observed_energy_kWh")))

# quintiles separate
ggplot(median_long_energy_saver_quintile_by_temp_bin[fuel == "gas" & variable != "savings_kWh"],
       aes(x = mean_temp,
           y = median_energy,
           colour = factor(variable))) + 
  geom_point() + 
  geom_line(aes(linetype = factor(variable))) +
  ggtitle("Gas") + 
  facet_grid(.~factor(saving_perc_quintile)) + 
  ylim(c(0, 80))

ggplot(median_long_energy_saver_quintile_by_temp_bin[fuel == "elec" & variable != "savings_kWh"],
       aes(x = mean_temp,
           y = median_energy,
           colour = factor(variable))) + 
  geom_point() + 
  geom_line(aes(linetype = factor(variable))) +
  ggtitle("Electricity") + 
  facet_grid(.~factor(saving_perc_quintile)) + 
  ylim(c(0, 10))

ggplot(median_long_energy_saver_quintile_by_temp_bin[fuel == "gas" & 
                                                       variable == "savings_kWh"],
       aes(x = mean_temp,
           y = median_energy,
           colour = factor(saving_perc_quintile))) + 
  geom_point() + 
  geom_line(aes(linetype = factor(variable))) +
  ggtitle("Gas") 


# By consumer quintile -----------------------------------------------------

median_long_energy_consumer_quintile <- long_energy_temp_range[N == n_included_bins, 
                                                            .(value = median(value),
                                                              mean_2m_temp_C = mean(mean_2m_temp_C)),
                                                            keyby = .(fuel, PUPRN, variable, bin, 
                                                                      tot_obs_quintile)]

median_long_energy_consumer_quintile_by_temp_bin <- median_long_energy_consumer_quintile[, 
                                                                                   .(mean_temp = mean(mean_2m_temp_C),
                                                                                     mean_energy = mean(value, na.rm = TRUE),
                                                                                     median_energy = calc_sdc_percentiles(value, 0.5)$approx,
                                                                                     low_iqr = calc_sdc_percentiles(value, 0.25)$approx,
                                                                                     high_iqr = calc_sdc_percentiles(value, 0.75)$approx,
                                                                                     N = .N,
                                                                                     N_PUPRN = length(unique(PUPRN))),
                                                                                   keyby = .(fuel, variable, tot_obs_quintile, bin)]

# All quintiles together
ggplot(median_long_energy_consumer_quintile_by_temp_bin[fuel == "gas" & variable != "savings_kWh"],
       aes(x = mean_temp,
           y = median_energy,
           colour = factor(tot_obs_quintile))) + 
  geom_point() + 
  geom_line(aes(linetype = factor(variable))) +
  ggtitle("Gas") + 
  facet_grid(.~factor(variable, levels = c("predicted_energy_kWh", "observed_energy_kWh")))


ggplot(median_long_energy_consumer_quintile_by_temp_bin[fuel == "elec" & variable != "savings_kWh"],
       aes(x = mean_temp,
           y = median_energy,
           colour = factor(tot_obs_quintile))) + 
  geom_point() + 
  geom_line(aes(linetype = factor(variable))) +
  ggtitle("Electricity")  + 
  facet_grid(.~factor(variable, levels = c("predicted_energy_kWh", "observed_energy_kWh")))

# quintiles separate
ggplot(median_long_energy_consumer_quintile_by_temp_bin[fuel == "gas" & variable != "savings_kWh"],
       aes(x = mean_temp,
           y = median_energy,
           colour = factor(variable))) + 
  geom_point() + 
  geom_line(aes(linetype = factor(variable))) +
  ggtitle("Gas") + 
  facet_grid(.~factor(tot_obs_quintile)) + 
  ylim(c(0, 130))

ggplot(median_long_energy_consumer_quintile_by_temp_bin[fuel == "elec" & variable != "savings_kWh"],
       aes(x = mean_temp,
           y = median_energy,
           colour = factor(variable))) + 
  geom_point() + 
  geom_line(aes(linetype = factor(variable))) +
  ggtitle("Electricity") + 
  facet_grid(.~factor(tot_obs_quintile)) + 
  ylim(c(0, 15))




# Compare quintile 5 with the rest ----------------------------------------

saver_quintile_5_vs_rest <- long_energy_temp_range[N == n_included_bins, 
                                                   .(value = median(value),
                                                     mean_2m_temp_C = mean(mean_2m_temp_C)),
                                                   keyby = .(fuel, PUPRN, variable, bin, 
                                                             saving_perc_quintile == 5)]
setnames(saver_quintile_5_vs_rest, "saving_perc_quintile", "saving_perc_quintile_is_5")

saver_quintile_5_vs_rest_by_temp_bin <- saver_quintile_5_vs_rest[,
                                                                 .(
                                                                   mean_temp = mean(mean_2m_temp_C),
                                                                   mean_energy = mean(value, na.rm = TRUE),
                                                                   median_energy = calc_sdc_percentiles(value, 0.5)$approx,
                                                                   low_iqr = calc_sdc_percentiles(value, 0.25)$approx,
                                                                   high_iqr = calc_sdc_percentiles(value, 0.75)$approx,
                                                                   N = .N,
                                                                   N_PUPRN = length(unique(PUPRN))
                                                                 ),
                                                                 keyby = .(fuel, variable, saving_perc_quintile_is_5, bin)]

ggplot(saver_quintile_5_vs_rest_by_temp_bin[fuel == "gas" & variable != "savings_kWh"],
       aes(x = mean_temp,
           y = median_energy,
           colour = factor(saving_perc_quintile_is_5))) + 
  geom_point() + 
  geom_line(aes(linetype = factor(variable))) +
  ggtitle("Gas") + 
  facet_grid(.~factor(saving_perc_quintile_is_5)) + 
  ylim(c(0, 80))


# save --------------------------------------------------------------------

fwrite(median_energy_by_temp_bin,
       paste0("./data/outputs/rq2/rq2_median_energy_by_temp_bin.csv"))
fwrite(long_median_energy_by_temp_bin,
       paste0("./data/outputs/rq2/rq2_perc_reduction_by_temp_bin.csv"))
fwrite(median_long_energy_saver_quintile_by_temp_bin,
       paste0("./data/outputs/rq2/rq2_median_energy_by_temp_bin_saver_quintile.csv"))
fwrite(median_long_energy_consumer_quintile_by_temp_bin,
       paste0("./data/outputs/rq2/rq2_median_quintile_by_temp_bin_energy_consumer.csv"))
fwrite(saver_quintile_5_vs_rest_by_temp_bin,
       paste0("./data/outputs/rq2/rq2_median_energy_by_temp_bin_saver_quintile_5_vs_rest.csv"))
