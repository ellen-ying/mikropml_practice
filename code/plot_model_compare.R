library(tidyverse)
library(wesanderson)

read_performance <- function(file_name) {
  
  read_tsv(file_name) %>% 
    mutate(method = str_replace(file_name, ".*/(.*)_performance.tsv", "\\1"))
  
}

performance_file <- 
  list.files("data/processed_full", pattern = "performance.tsv", full.names = TRUE)

# l2_genus <- read_tsv("data/processed_full/l2_genus_performance.tsv") %>% 
#   mutate(method = "l2_genus")
# 
# l2_genus_fit <- read_tsv("data/processed_full/l2_genus_fit_performance.tsv") %>% 
#   mutate(method = "l2_genus_fit")
# 
# rf_genus <- read_tsv("data/processed_full/rf_genus_performance.tsv") %>% 
#   mutate(method = "rf_genus")
# 
# rf_genus_fit <- read_tsv("data/processed_full/rf_genus_fit_performance.tsv") %>% 
#   mutate(method = "rf_genus_fit")

map_dfr(performance_file, ~ read_performance(.x)) %>% 
#bind_rows(l2_genus, l2_genus_fit, rf_genus, rf_genus_fit) %>% 
  select(method, cv_metric_AUC, AUC) %>% 
  rename(training = cv_metric_AUC,
         testing = AUC) %>% 
  pivot_longer(c(training, testing), 
               names_to = "training_testing", 
               values_to = "AUC") %>%
  mutate(training_testing = as_factor(training_testing) %>% 
           fct_relevel("training"),
         model = str_remove(method, "_genus.*"),
         data = str_extract(method, "genus.*"),
         ) %>% 
  ggplot(aes(x = data, y = AUC, color = training_testing)) +
  #geom_boxplot() +
  geom_hline(yintercept = 0.65, color = "gray", linetype = "dashed") +
  stat_summary(fun.data = "median_hilow", 
               geom = "pointrange",
               fun.args = list(conf.int = 0.5),
               position = position_dodge(width = 0.5)) +
  scale_x_discrete(label = c("Genus", "Genus+Fit")) +
  scale_color_manual(name = NULL,
                     breaks = c("training", "testing"),
                     labels = c("Training", "Testing"),
                     values = wes_palette("Royal1")
                     ) +
  lims(y = c(0.5, 1)) +
  labs(x=NULL,
       y = "Area under the receiver\noperator characteristic curve") +
  facet_wrap(~ model, ncol = 2, strip.position = "bottom",
             labeller = labeller(
               model = c(l2 = "L2 Regularized\nLogistic Regression", 
                         rf = "Random Forest")
             )) +
  theme_classic() +
  theme(
    strip.placement = "outside",
    strip.background = element_blank()
  )

ggsave("figures/model_compare.pdf", width = 5, height = 4)
