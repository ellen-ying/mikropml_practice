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

srn_genus_results <- 
  run_ml(srn_genus_data,
         method = "glmnet",
         outcome_colname = "srn",
         kfold = 5, # five-fold cross-validation
         cv_times = 100, # iteration times
         training_frac = 0.8, # training set = 80% of the dataset
         seed = 19990331)