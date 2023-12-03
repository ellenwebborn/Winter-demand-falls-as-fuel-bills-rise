get_action_change_by_quintile <- function(action = "A3", 
                                          valid_responses = c("Yes", "No"),
                                          single_response = "Yes",
                                          dt = colc_survey) {
  
  all_counts <- rbind(data.table(saving_perc_quintile = 0,
                                 tot = colc_survey[get(action) %in% valid_responses, .N]),
                      colc_survey[get(action) %in% valid_responses, .(tot = .N), 
                                  keyby = saving_perc_quintile])
  
  single_response_count <- rbind(data.table(saving_perc_quintile = 0,
                                            N = colc_survey[get(action) == single_response, .N]),
                                 colc_survey[get(action) == single_response, .N, 
                                             keyby = .(saving_perc_quintile)])
  
  output <- all_counts[single_response_count, on = "saving_perc_quintile"]
  output[, `:=`(perc = round(N / tot * 100, 3), 
                response = single_response,
                variable = action)]
  
  setcolorder(output, c("variable", "response", "saving_perc_quintile", "tot", "N", "perc"))
}