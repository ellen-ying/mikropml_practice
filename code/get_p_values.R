library(tidyverse)

# we need to compare the observed group difference with
# the theoretical difference if there the two were truly not different from each other
# to see if the observed one is higher than the null difference

get_diff <- function(df) {
  
  # observed difference
  df %>% 
    group_by(condition) %>% 
    summarize(mean = mean(AUC)) %>% 
    summarize(diff = abs(diff(mean))) %>% 
    pull(diff)
  
}

get_null_diff <- function(df) {
  
  # null
  df %>% 
    # shuffle the AUC value
    mutate(AUC = sample(AUC, nrow(auc_comparison))) %>% 
    get_diff
  
}

perm_p_value <- function(df) {
  #df <- auc_comparison
  obs <- get_diff(df)
  iteration <- 1000
  null <- map_dbl(1:iteration, ~ get_null_diff(df))
  r <- sum(obs <= null)
  p <- (r + 1)/(iteration + 1)
  print(p)
}
  
#perm_p_value(auc_comparison)
