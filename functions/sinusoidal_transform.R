#' Function to take transform variables with sinusoidal output
#' Outputs a data table with sin transform and cos transform
#' 
#' 
#' @param dt is the data table of input and output variables
#' @param x the column to be transformed
#' @param x_colname the name of the pre-transformed column in the output table
#' @param sin_colname the name of the sin transform column in the output table
#' @param cos_colname the name of the cos transform column in the output table
#' @param X scale factor, leave as NULL if no scaling - ie transforms in [-1, 1]

#' @author Ellen Zapata-Webborn \email{e.webborn@ucl.ac.uk} (original)


sinusoidal_transform <- function(x, 
                                 x_colname,
                                 sin_colname = "sin_x",
                                 cos_colname = "cos_x",
                                 X = NULL) {
  
  if(is.null(X)) {X = max(x)}
  
  sin_x <- 0.5 * sin(2 * pi * x / X) + 0.5
  cos_x <- 0.5 * cos(2 * pi * x / X) + 0.5
  
  output <- data.table(x,
                       sin_x,
                       cos_x)
  setnames(output,
           c(x_colname, sin_colname, cos_colname))
  
  setkeyv(output, x_colname)
  return(output)
}


