library(tidyverse)
library(mikropml)

read_tsv("data/processed/l2_genus_pooled_hp.tsv") %>% 
  # plot the performance of difference hyperparameters
  plot_hp_performance(lambda, AUC)

# find the best performing hyperparameters
read_tsv("data/processed/l2_genus_pooled_hp.tsv") %>% 
  group_by(alpha, lambda) %>% 
  summarize(mean_AUC = mean(AUC), 
            lquartile = quantile(AUC, probs = 0.25),
            lquartile = quantile(AUC, probs = 0.75),
            .groups = "drop") %>% 
  # select the row with the highest value of mean_AUC
  top_n(mean_AUC, n = 3)

read_tsv("data/processed/l2_genus_pooled_performance.tsv") %>% 
  select(seed, cv_metric_AUC, AUC) %>% 
  pivot_longer(-seed, names_to = "metric", values_to = "AUC") %>% 
  ggplot(aes(x = metric, y = AUC)) +
  geom_boxplot()
