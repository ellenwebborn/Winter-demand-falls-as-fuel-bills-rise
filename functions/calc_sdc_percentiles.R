#' Function to approximate the percentiles of a vector of values by taking the mean
#' of a set of 10 values closest to the percentile
#' 
#' Outputs a data table (inherits from data frame) with 3 columns:
#'    percentile: the percentiles calculated, e.g. median is "50%"
#'    approx: the approximation for the value of this percentile

#' @param input_data vector of numbers that we want an approximate median for. Must be 
#'   at least 10 values. If 10 the mean will be used. 
#' @percentiles vector or single value of points between 0 and 1, e.g. median = 0.5
#' @param vector_out if only a vector of percentiles is wanted set to TRUE, otherwise outputs data table
#' 
#' @author Ellen Zapata-Webborn \email{e.webborn@ucl.ac.uk} (original)


calc_sdc_percentiles <- function(input_vector,
                                 percentiles = c(0.25, 0.5, 0.75),
                                 vector_out = FALSE) {
  library(data.table)
  # Remove NAs
  v <- input_vector[!is.na(input_vector) & !is.infinite(input_vector)]
  
  # Check for input errors
  if(length(v) < 10) {
    stop("fewer than 10 points excluding NAs")}
  
  
  # calculate the values we need to approximate/pass SDC check
  true_quantiles <- quantile(v, percentiles)
  n_quantiles <- length(percentiles)
  
  # initialise output table
  outputs <- data.table(percentile = names(true_quantiles),
                        approx = rep(NA_real_, n_quantiles))
  
  
  # approximate percentiles
  for(i in 1:n_quantiles) {
    tmp <- data.table(data_point = v)
    tmp[, abs_diff := abs(data_point - true_quantiles[i])]
    setorder(tmp, abs_diff)
    outputs[i, approx := tmp[1:10, mean(data_point)]]
  }
  
  if(vector_out == FALSE) {
    return(outputs)
  } else {
    return(outputs$approx)
  }
}
