library(tidyverse)
source("code/get_p_values.R")

genus <- 
  read_tsv("data/processed/l2_genus_pooled_performance.tsv") %>% 
  mutate(condition = "genus")

genust_fit <- 
  read_tsv("data/processed/l2_fit_genus_pooled_performance.tsv") %>% 
  mutate(condition = "genus_fit")

auc_comparison <- 
  bind_rows(genus, genust_fit) %>% 
  select(AUC, condition)

auc_comparison %>% 
  ggplot(aes(x = AUC, fill = condition)) +
  geom_density(alpha = 0.5) + 
  stat_summary(aes(x = 0.7, y = AUC, xintercept = stat(y)),
               fun = mean, geom = "vline") +
  labs(y = "Density")

ggsave("figures/genus_fit_auc_comparison.pdf", width = 5, height = 4)

auc_comparison %>% 
  group_by(condition) %>% 
  summarize(mean = mean(AUC)) %>% 
  pivot_wider(names_from = "condition", values_from = "mean") %>% 
  mutate(diff = abs(genus - genus_fit))

p.value <- perm_p_value(auc_comparison)
