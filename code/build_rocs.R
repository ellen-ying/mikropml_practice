library(tidyverse)
library(wesanderson)

get_sens_spec_lookup <- function(data){
  
  total <- 
    data %>% 
    group_by(observed) %>% 
    count() %>% 
    pivot_wider(names_from = "observed", values_from = "n")
  
  data %>% 
    arrange(desc(srn)) %>% 
    mutate(
      is_srn = (observed == "srn"),
      # after the data is arranged, the true positives at a threshold are the number
      # of cases that is srn among cases with a predicted rate above that value
      tp = cumsum(is_srn),
      # the false positives at this threshold are the number of cases that is srn among
      # cases with a predicted rate above that value
      fp = cumsum(!is_srn),
      # aka the true positive rate
      sensitivity = tp / total$srn,
      fpr = fp / total$healthy,
      specificity = 1 - fpr
    ) %>% 
    select(sensitivity, specificity, fpr)
  
}

get_sensitivity <- function(x, data) {
  
  data %>% 
    # among the specificity higher than the given value
    filter(specificity >= x) %>% 
    # we select the highest sensitivity value
    top_n(sensitivity, n = 1) %>% 
    mutate(specificity = x, fpr = 1 - x) %>% 
    # remove duplicate values
    distinct()
  
}

get_pooled_sens_spec <- function(file_name, specificity) {
  
  model <- readRDS(file_name)
  
  prob <- predict(model$trained_model, model$test_data, type = "prob")
  observed <- model$test_data$srn
  
  prob_obs <- bind_cols(prob, observed = observed) %>% 
    select(-healthy)
  
  sens_spec_lookup <- get_sens_spec_lookup(prob_obs)
  
  # get sens and spec from the pooled data
  map_dfr(specificity, ~ get_sensitivity(.x, sens_spec_lookup)) %>% 
    mutate(model = str_replace(file_name, "data/processed/(.*)_\\d*.Rds", "\\1"))

}

# specify the specificity level
specificity <- seq(0, 1, 0.01)

#get_pooled_sens_spec("data/processed/l2_genus_1.Rds", specificity)

# get sens and spec from all output files
pooled_sens_spec <- 
  list.files(path = "data/processed", pattern = ".*\\d*.Rds", full.names = TRUE) %>% 
  map_dfr(get_pooled_sens_spec, specificity)

pooled_sens_spec %>% 
  as_tibble() %>% 
  group_by(model, specificity) %>% 
  summarize(
    l_quartile = quantile(sensitivity, prob = 0.25),
    u_quartile = quantile(sensitivity, prob = 0.75),
    sensitivity = median(sensitivity),
    .groups = "drop"
  ) %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity, 
             ymin = l_quartile, ymax = u_quartile)) +
  geom_ribbon(aes(fill = model), alpha = 0.25) +
  #geom_line(aes(color = model)) +
  geom_step(aes(color = model)) +
  scale_color_manual(name = NULL,
                     breaks = c("l2_genus", "l2_genus_fit", "rf_genus"),
                     labels = c("L2 regression on\ngenus data", 
                               "L2 regression on\ngenus + fit data", 
                               "Random forest on\ngenus data"),
                     values = wes_palette("Darjeeling1", 3)
                     ) +
  scale_fill_manual(name = NULL,
                     breaks = c("l2_genus", "l2_genus_fit", "rf_genus"),
                     labels = c("L2 regression on\ngenus data", 
                                "L2 regression on\ngenus + fit data", 
                                "Random forest on\ngenus data"),
                     values = wes_palette("Darjeeling1", 3)
                    ) +
  labs(x = "False positive rate",
       y = "True positive rate") +
  theme_classic() +
  theme(
    legend.position = c(0.8, 0.2),
    legend.key.size = unit(1.8,"lines")
  )

ggsave("figures/roc_curve.pdf", width = 5, height = 5)
