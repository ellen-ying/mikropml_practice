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

# train with preprocessed data
srn_genus_results <- 
  run_ml(srn_genus_preprocessed,
         # usinglogistic regression method
         method = "glmnet",
         outcome_colname = "srn",
         kfold = 5, # five-fold cross-validation
         cv_times = 100, # iteration times
         training_frac = 0.8, # training set = 80% of the dataset
         seed = 19990331)

