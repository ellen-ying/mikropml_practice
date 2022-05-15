#!/usr/bin/env Rscript --vanilla

source("code/genus_process.R")
library(mikropml)

args <- commandArgs(trailingOnly = TRUE)
output_file <- args[1]
#output_file <- c("data/processed/l2_genus_fit_1.Rds", "data/processed/l2_genus_fit_2.Rds")
seed <- as.numeric(str_replace(output_file, ".*_(\\d*)\\.Rds", "\\1"))
feature_script <- args[2]

source(feature_script)

srn_genus_data <-
  composite %>%
  feature_select() %>%
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

# train with preprocessed data
model <- run_ml(srn_genus_preprocessed,
                # using logistic regression method
                method = approach,
                outcome_colname = "srn",
                kfold = 5, # five-fold cross-validation
                cv_times = 100, # iteration times
                training_frac = 0.8, # training set = 80% of the dataset
                hyperparameters = hyperparameter,
                seed = seed)

# save into an RDS file
saveRDS(model, file = output_file)