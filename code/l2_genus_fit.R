feature_select <- function(df) {
  
  df %>%
    select(group, taxonomy, rel_abund, srn, fit_result)
  
}

# specify the hyperparameters that are fed into the cross-validation process
# each combination of hyperparameters will go through cv for 100 times
hyperparameter <- list(alpha = 0,
                       lambda = c(0.1, 1, 3, 5, 10))

approach <- "glmnet"