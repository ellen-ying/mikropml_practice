library(tidyverse); library(here)

shared <- 
  # locate the file
  here("data/raw/baxter.subsample.shared") %>% 
  # read file, specify the coloumn type
  read_tsv(col_types = cols(Group = col_character(),
                            .default = col_double())
          ) %>% 
  # clean the name, select the columns we want
  rename_all(tolower) %>% 
  select(group, starts_with("otu")) %>% 
  # tidy up the data into a long format
  pivot_longer(cols = -group, names_to = "otu", values_to = "count")

taxonomy <- 
  here("data/raw/baxter.cons.taxonomy") %>% 
  read_tsv() %>% 
  rename_all(tolower) %>% 
  select(otu, taxonomy) %>% 
  mutate(
    otu = tolower(otu),
    # replace multiple cases that match the regular expression
    # \\d+ means matching one or more digits
    taxonomy = str_replace_all(taxonomy, "\\(\\d+\\)", ""),
    # replace the first case that matches the expression
    taxonomy = str_replace(taxonomy, ";unclassified", "_unclassified"),
    taxonomy = str_replace_all(taxonomy, ";unclassified", ""),
    # ;$ matches the semicolon at the end of a string
    taxonomy = str_replace_all(taxonomy, ";$", ""),
    # .*; matches everything up to a semicolon
    taxonomy = str_replace_all(taxonomy, ".*;", "")
    ) 

metadata <- 
  here("data/raw/baxter.metadata.tsv") %>% 
  read_tsv(col_types = cols(sample = col_character())) %>% 
  rename_all(tolower) %>% 
  mutate(
    # two ways to doing the coding, obviously one is more efficient
    #srn = ifelse(dx_bin %in% c("Adv Adenoma", "Cancer"), TRUE, FALSE)
    srn = dx_bin == "Adv Adenoma" | dx_bin == "Cancer",
    lesion = dx_bin == "Adv Adenoma" | dx_bin == "Cancer" | dx_bin == "Adenoma"
    )
  
inner_join(shared, taxonomy, by = "otu") %>% 
  # count the total number of taxonomy in each group
  group_by(group, taxonomy) %>% 
  summarize(count = sum(count), .groups = "drop") %>% 
  group_by(group) %>% 
  # sum over each group to calculate the relative abundance
  mutate(rel_abund = count / sum(count)) %>% 
  ungroup() %>% 
  select(-count) %>% 
  inner_join(., metadata, by = c("group" = "sample"))
