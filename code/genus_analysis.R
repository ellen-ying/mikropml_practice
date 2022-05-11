library(tidyverse)
library(here)
library(broom)
library(ggplot2)
library(ggtext)
library(wesanderson)

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
  
composite <- 
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

sig_genera <- 
  composite %>% 
  # nest data by taxonomy
  nest(data = -taxonomy) %>% 
  # run wilcox.test for each taxonomy
  mutate(
    test = map(.x = data, ~ wilcox.test(rel_abund ~ srn, data = .x) %>% tidy)
  ) %>% 
  unnest(test) %>% 
  # adjust for multiple comparisons
  mutate(p.adjust = p.adjust(p.value, "BH")) %>% 
  filter(p.adjust < .05) %>% 
  select(taxonomy, p.adjust)

composite %>% 
  inner_join(sig_genera, by = "taxonomy") %>% 
  mutate(
    # add a small value to rel_abund so that the scale can be log-tranformed
    rel_abund = 100 * (rel_abund + 1/20000),
    # put stars so that the text can be formatted using markdown syntax\
    # \\1 matches the pattern in (.*)
    taxonomy = str_replace(taxonomy, "(.*)", "*\\1*"),
    taxonomy = str_replace(taxonomy, "\\*(.*)_unclassified\\*", "Unclassified<br>*\\1*"),
    srn = factor(srn, levels = c(TRUE, FALSE))
    ) %>% 
  ggplot(aes(x = rel_abund, y = taxonomy, color = srn, fill = srn)) +
  # plot points, jittering and dodging at the same time
  geom_jitter(position = position_jitterdodge(dodge.width = 0.8,
                                              jitter.width = 0.3)) +
  # median and confidence interval of 50%, which are the quartiles
  stat_summary(fun.data = median_hilow, fun.args = list(conf.int = 0.5),
               geom = "pointrange", position = position_dodge(width = 0.8),
               # set the color to black, turn of the legend
               color = "black", show.legend = FALSE) +
  labs(x = "Relative abundance (%)", y = NULL) +
  scale_color_manual(NULL,
                    breaks = c(FALSE, TRUE),
                     values = wes_palette("Royal1"),
                     labels = c("Healthy", "SRN")) +
  scale_fill_manual(NULL,
                     breaks = c(FALSE, TRUE),
                     values = wes_palette("Royal1"),
                     labels = c("Healthy", "SRN")) +
  scale_x_log10() +
  theme_classic() +
  theme(
    axis.text.y = element_markdown()
  )

ggsave("figures/significant_genera.pdf", width = 6, height = 4)
