library(tidyverse)
library(ggtext)

l2_files <- list.files(path = "data/processed", 
                       pattern = "l2_genus_\\d*.Rds",
                       full.names = TRUE)

#test <- l2_files[1]

get_weights <- function(file_name) {

model <- readRDS(file_name) %>% 
  pluck("trained_model")

# get the coefficients of the best tuned model
coef(model$finalModel, model$bestTune$lambda) %>% 
  as.matrix() %>% 
  # convert the rownames into a column
  as_tibble(rownames = "feature") %>% 
  rename(weight = s1) %>% 
  mutate(
    seed = str_replace(file_name, "data/processed/l2_genus_(\\d*).Rds", "\\1")
    )

}

l2_weights <- map_dfr(l2_files, ~ get_weights(.x))

l2_weights %>% 
  filter(feature != "(Intercept)") %>% 
  group_by(feature) %>% 
  # calculate the medians and interquartile range
  summarize(
    median = median(weight),
    l_quartile = quantile(weight, prob = 0.25),
    u_quartile = quantile(weight, prob = 0.75)
  ) %>% 
  mutate(
    feature = paste0("*", feature, "*"),
    feature = str_replace(feature, "\\*(.*)_unclassified\\*", 
                          "Unclassified \\*\\1\\*"),
    feature = fct_reorder(feature, median),
    ) %>% 
  filter(abs(median) >= 0.01) %>% 
  ggplot(aes(x = median, y = feature, 
             # aesthetics taken by geom_range function
             xmin = l_quartile, xmax = u_quartile)) +
  geom_point() +
  geom_linerange() +
  geom_vline(xintercept = 0, color = "gray") +
  labs(y = NULL,
       x = "Weights") +
  theme_classic() +
  theme(
    axis.text.y = element_markdown()
  )

ggsave("figures/l2_weights.pdf", width = 5, height = 6)
