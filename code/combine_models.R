#!/usr/bin/env Rscript --vanilla

library(mikropml)
library(tidyverse)

args <- commandArgs(trailingOnly = TRUE)

output_file <- tail(args, 1)
rds_files <- args[1:length(args)-1]

# rds_file <- c("data/processed/l2_genus_1.Rds",
# "data/processed/l2_genus_2.Rds",
# "data/processed/l2_genus_3.Rds")

iterative_run_ml_results <- map(rds_files, readRDS)

get_output_file <- function(x, output_file) {
  
  if (str_detect(output_file, "hp")) {
    #print("hp")
    x %>%
      # use the pluck function to get the part we want
      # the pluck function is from purrr package
      map(pluck, "trained_model") %>%
      # combined results
      combine_hp_performance() %>%
      pluck("dat") %>%
      write_tsv(output_file)
    
  } else {
    #print("perf")
    x %>%
      # use the pluck function to get the part we want
      # the pluck function is from purrr package
      map_dfr(pluck, "performance") %>%
      write_tsv(output_file)
    
  }
  
}

get_output_file(iterative_run_ml_results, output_file)

