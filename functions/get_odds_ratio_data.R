# for use in rq6

get_odds_ratio_data <- function(dt, 
                                variable, 
                                name = NA) {
  
  if(is.na(name)) {name <- variable}
  OR <- epitools::oddsratio(table(dt[, .(get(variable), group)]))
  output <- cbind(data.table(variable = name,
                             subgroup = rownames(OR$measure)),
                  OR$measure,
                  OR$p.value)[, 1:6]
  output[, `:=`(estimate = round(estimate, 4),
                lower = round(lower, 4),
                upper = round(upper, 4))]
}