# Prep calendar data for use in cost of living crisis modelling


# Import and combine data -------------------------------------------------

source("./scripts/setup_colc.R")
source("./scripts/functions/add_day_info.R")
source("./scripts/functions/sinusoidal_transform.R")


# create vector for all the training and testing dates
all_dates <- c(seq(as.Date("2021-10-01"), 
                   as.Date("2022-03-31"), 
                   by = "day"),
               seq(as.Date("2022-10-01"), 
                   as.Date("2023-03-31"), 
                   by = "day"))
all_dates <- data.table(date = all_dates)

with_day_info <- add_day_info(all_dates,
                              date_col = "date",
                              year_range = 2021:2023)

with_day_info[, weekend_or_holiday := (weekend==TRUE | holiday == TRUE)]
setcolorder(with_day_info, "date")
with_day_info[, `:=`(wkyr = NULL)]

saveRDS(with_day_info, 
        file = paste0(input_data_folder, "calendar_variables.RDS"))
