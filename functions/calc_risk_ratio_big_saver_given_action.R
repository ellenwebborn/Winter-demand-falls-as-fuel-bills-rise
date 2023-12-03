
calc_risk_ratio_big_saver_given_action <- function(action = "A12",
                                                   valid_responses = c("A great deal of effort",
                                                                       "A little effort",
                                                                       "No effort at all",
                                                                       "Some effort"),
                                                   single_response = "A great deal of effort",
                                                   dt = colc_survey,
                                                   outcome_var = "is_saver_q5") {
  input <- dt[get(action) %in% valid_responses]
  input[, exposure_var := ifelse(get(action) == single_response, TRUE, FALSE)]
  risk_ratio <- data.table(epitab(table(input[, .(exposure_var, 
                                                  get(outcome_var))]),
                                  method = "riskratio")$tab)
  setnames(risk_ratio,
           c("FALSE", "TRUE", "p0", "p1"),
           c("N_not_big_savers", "N_big_savers", "perc_not_big_savers", "perc_big_savers"))
  output <- cbind(data.table(variable = action,
                             response = c(paste0("Not ", single_response),
                                          single_response)),
                  risk_ratio)
  output[, `:=`(perc_not_big_savers = round(perc_not_big_savers * 100, 3),
                perc_big_savers = round(perc_big_savers * 100, 3),
                riskratio = round(riskratio, 3),
                lower = round(lower, 3),
                upper = round(upper, 3))]
  
}


calc_risk_ratio_big_saver_given_characteristics <- function(characteristic = "EPC_rating",
                                                            valid_responses = NA,
                                                            invalid_responses = NA,
                                                            dt = all_data,
                                                            outcome_var = "group",
                                                            name = NA) {
  # filter on either valid_responses or filter out invalid_responses, at least one is NA
  if(!is.na(valid_responses)) {
    dt <- dt[get(characteristic) %in% valid_responses]
  } else if(!is.na(invalid_responses)) {
      dt <- dt[!get(characteristic) %in% invalid_responses]
  }
  dt <- dt[!is.na(get(characteristic))]
  if(is.na(name)) {name <- characteristic}
  
  unformatted_risk_ratio <- epitab(table(dt[, .(get(outcome_var), get(characteristic))]),
                                   method = "riskratio")$tab
  risk_ratio <- cbind(data.table(variable = name,
                                 saver_group = rownames(unformatted_risk_ratio),
                                 subgroup = colnames(unformatted_risk_ratio)[c(1, 3)]),
                      data.table(unformatted_risk_ratio))
  colnames(risk_ratio)[c(4, 6)] <- c("N_subgroup1", "N_subgroup2")
  return(risk_ratio)
  
}
