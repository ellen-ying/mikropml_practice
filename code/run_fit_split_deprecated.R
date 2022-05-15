#!/usr/bin/env Rscript --vanilla

source("code/genus_process.R")
library(mikropml)

seed <- commandArgs(trailingOnly = TRUE) %>% as.numeric

srn_genus_data <- 
  composite %>% 
  select(group, taxonomy, rel_abund, srn, fit_result) %>% 
  # machine learning tools need the features (independent variables) to be in separate columns
  pivot_wider(names_from = "taxonomy", values_from = "rel_abund") %>% 
  select(-group) %>% 
  # machine learning tools cannot handel logical values
  mutate(srn = ifelse(srn, "srn", "healthy")) %>% 
  select(srn, everything())

# preprocessing data
# rescaling fit_result and weight
# remove features contains near-zero variance
srn_genus_preprocessed <- 
  preprocess_data(srn_genus_data, outcome_colname = "srn")$dat_transformed

# specify the hyperparameters that are fed into the cross-validation process
# each combination of hyperparameters will go through cv for 100 times
test_hp <- list(alpha = 0,
                lambda = c(0.1, 1, 3, 5, 10))

# train with preprocessed data
model <- run_ml(srn_genus_preprocessed,
                # using logistic regression method
                method = "glmnet",
                outcome_colname = "srn",
                kfold = 5, # five-fold cross-validation
                cv_times = 100, # iteration times
                training_frac = 0.8, # training set = 80% of the dataset
                hyperparameters = test_hp,
                seed = seed)

# save into an RDS file
saveRDS(model, file = paste0("data/processed/l2_fit_genus_", seed, ".Rds"))