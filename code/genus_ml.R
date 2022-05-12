source("code/genus_process.R")
library(mikropml)

srn_genus_data <- 
  composite %>% 
  select(group, taxonomy, rel_abund, srn) %>% 
  # machine learning tools need the features (independent variables) to be in separate columns
  pivot_wider(names_from = "taxonomy", values_from = "rel_abund") %>% 
  select(-group) %>% 
  # machine learning tools cannot handel logical values
  mutate(srn = ifelse(srn, "srn", "healthy")) %>% 
  select(srn, everything())

# practice <- 
#   composite %>% 
#   select(group, fit_result, site, gender, srn, weight) %>% 
#   distinct() %>% 
#   # code 0 weight as NA value
#   mutate(weight = na_if(weight, 0)) %>% 
#   select(-group)

# preprocessing data
# rescaling fit_result and weight
# remove features contains near-zero variance
srn_genus_preprocessed <- 
  preprocess_data(srn_genus_data, outcome_colname = "srn")$dat_transformed

srn_genus_results_no_pp <- 
  run_ml(srn_genus_data,
         # usinglogistic regression method
         method = "glmnet",
         outcome_colname = "srn",
         kfold = 5, # five-fold cross-validation
         cv_times = 100, # iteration times
         training_frac = 0.8, # training set = 80% of the dataset
         seed = 19990331)

# specify the hyperparameters that are fed into the cross-validation process
# each combination of hyperparameters will go through cv for 100 times
test_hp <- list(alpha = 0,
                lambda = c(0.1, 1, 3, 5, 10))

# train with preprocessed data
srn_genus_results <- 
  run_ml(srn_genus_preprocessed,
         # using logistic regression method
         method = "glmnet",
         outcome_colname = "srn",
         kfold = 5, # five-fold cross-validation
         cv_times = 100, # iteration times
         training_frac = 0.8, # training set = 80% of the dataset
         hyperparameters = test_hp,
         seed = 19990331)

# construct a function to run different splits on the data
get_srn_genus_results <- function(seed) {
  run_ml(srn_genus_preprocessed,
         # using logistic regression method
         method = "glmnet",
         outcome_colname = "srn",
         kfold = 5, # five-fold cross-validation
         cv_times = 100, # iteration times
         training_frac = 0.8, # training set = 80% of the dataset
         hyperparameters = test_hp,
         seed = seed)
}

library(tictoc)
library(furrr)

#plan("sequential") # serial processing, not parallel
#plan("multicore") # this does not work with windows or R studio
plan("multisession")

# use tictoc package to get the running time
tic()
# using seed 1, 2, 3 to split the data
#iterative_run_ml_results <-map(c(1, 2, 3), ~ get_srn_genus_results(.x))
# run map function in parallel
iterative_run_ml_results <-future_map(c(1, 2, 3), ~ get_srn_genus_results(.x),
                                      .option = furrr_options(seed = TRUE))
toc()

performance <- 
  iterative_run_ml_results %>% 
  # use the pluck function to get the part we want
  # the pluck function is from purrr package
  map(pluck, "trained_model") %>% 
  # combined results
  combine_hp_performance()

# plot the performance of difference hyperparameters
plot_hp_performance(performance$dat, lambda, AUC)

# find the best performing hyperparameters
performance$dat %>% 
  group_by(alpha, lambda) %>% 
  summarize(mean_AUC = mean(AUC), .groups = "drop") %>% 
  # select the row with the highest value of mean_AUC
  top_n(mean_AUC, n = 1)

# this function gets you the default hyperparameters values for the training
get_hyperparams_list(srn_genus_preprocessed, "glmnet")
