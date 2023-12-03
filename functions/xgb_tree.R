#' Function to take training/testing data and prediction set and 
#' output predictions with estimated errors
#' 
#' Works for one participant individually, designed to be called from
#' train_colc_models() which is called in parallel fron 01_train_models.R
#' 
#' @param formula regression formula for use in model
#' @param dt train/test set (data table)
#' @param n_folds number of folds for cross-validation (if using)
#' @param xgbGrid hyper_params_to_test defined in setup_colc.R of form expand.grid()
#' @param split_method defaults to cross-validation, other splits possible
#'
#' @author Ellen Zapata-Webborn \email{e.webborn@ucl.ac.uk} (original)
#'

xgb_tree <- function(formula = reg_formula,
                     dt = train_test,
                     n_folds = kfolds,
                     xgbGrid = hyper_params_to_test,
                     split_method = "cv") {

  if(split_method == "cv") {
    train(
      formula,
      data = dt,
      method = "xgbTree",
      preProcess = c('center', 'scale'),
      trControl = trainControl(
        method = split_method,
        number = n_folds,
        savePredictions = "final",
        predictionBounds = c(0, NA),
      ),
      tuneGrid = xgbGrid
    )
  } else {
    train(
      formula,
      data = dt,
      method = "xgbTree",
      preProcess = c('center', 'scale'),
      trControl = trainControl(
        method = split_method,
        savePredictions = "final",
        predictionBounds = c(0, NA)
      ),
      tuneGrid = xgbGrid
    )
  }
    

}

