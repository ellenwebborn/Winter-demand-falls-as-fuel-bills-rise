#' Function to take training/testing data and fit xgbTree model
#' 
#' Works for one participant individually, designed to be called in parallel
#'  from 04_retrain_on_full_train_test_set.R
#' 
#' @param p participnat id ('PUPRN')
#' @param dt data table of training dataset
#' @param final_models data table of selected formula and selected hyperparameter
#'  values from cross validation
#' @param all_formulas list of all possible formulas
#' 
#' @author Ellen Zapata-Webborn \email{e.webborn@ucl.ac.uk} (original)
#'

xgb_tree_parallel <- function(p,
                              dt = training_final,
                              final_models = final_models,
                              all_formulas = all_formulas) {
  
  formula_string <- final_models[PUPRN == p, formula]
  my_formula <- all_formulas[formula_string][[1]]
  dt <- dt[p][[1]]
  
  xgbGrid <- final_models[PUPRN == p, .(nrounds,
                                        max_depth,
                                        eta,
                                        gamma,
                                        colsample_bytree,
                                        min_child_weight,
                                        subsample)]
  
  return(train(
      my_formula,
      data = dt,
      method = "xgbTree",
      preProcess = c('center', 'scale'),
      trControl = trainControl(method = "none",
                               predictionBounds = c(0, NA)),
      tuneGrid = xgbGrid
    )
  )
  
}

