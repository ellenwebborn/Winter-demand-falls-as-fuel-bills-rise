# function from stack overflow
# Tidies a caret train output just like broom::tidy does for lm output

tidy.train <- function(x, ...) {
  s <- summary(x, ...)
  out <- data.table(term = row.names(s$coefficients),
                    estimate = s$coefficients[, "Estimate"],
                    std.error = s$coefficients[, "Std. Error"],
                    statistic = s$coefficients[, "t value"],
                    p.value = s$coefficients[, "Pr(>|t|)"])
  row.names(out) <- NULL
  out
}