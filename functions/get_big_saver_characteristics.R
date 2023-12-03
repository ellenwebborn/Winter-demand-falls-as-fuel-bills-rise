# function for use in RQ6

get_big_saver_characteristics <- function(dt,
                                          variable,
                                          name = NA) {
  if(is.na(name)) {name <- variable}
  output <- dt[, .N, keyby = .(get(variable), group)]
  totals <- dt[, .(tot = .N), keyby = group]
  output <- totals[output, on = "group"]
  setnames(output, "get", "subgroup")
  output[, `:=`(variable = name,
                perc = round(N / tot * 100, 2))]
  setcolorder(output,
              c("variable", "subgroup", "group", "tot", "N", "perc"))
  return(output)
}