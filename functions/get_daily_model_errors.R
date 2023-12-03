

get_daily_model_errors <- function(pred_obs_data,
                                   obs_colname,
                                   pred_colname,
                                   label = "test") {
  dt <- pred_obs_data[, .(
    obs = mean(get(obs_colname)),
    pred = mean(get(pred_colname)),
    obs_min_pred = mean(get(obs_colname) -
                          get(pred_colname)),
    obs_min_pred_sqrd = mean(get(obs_colname) -
                               get(pred_colname)) ^ 2),
  keyby = .(Read_date_effective_local)]
  n <- nrow(dt)
  
  output <- data.table(fuel = pred_obs_data[1, fuel],
                       data = label,
                       N = n,
                       rmse = dt[, sqrt(sum(obs_min_pred_sqrd) / n)],
                       mape = dt[, sum(abs(obs_min_pred / obs)) / n * 100],
                       cv_rmse = dt[, sqrt(sum(obs_min_pred_sqrd) / (n-1)) / mean(obs) * 100],
                       nmbe = dt[, sum(obs_min_pred) / (n-1) / mean(obs) * 100])
  lm <- lm(pred ~ obs, dt)
  output[, rsqrd := summary(lm)$r.squared]
  return(output)
}
