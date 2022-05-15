feature_select <- function(df) {
  
  df %>%
    select(group, taxonomy, rel_abund, srn, fit_result)
  
}

# this is from the default parameter list that can be viewed by get_hyperparams_list()
hyperparameter <- list(mtry = c(8, 17, 34))

approach <- "rf"