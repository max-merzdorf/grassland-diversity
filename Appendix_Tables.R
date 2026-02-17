library(tidyverse)
library(xtable)

# Value stuffs
t <- read.csv(file = "./results/Species_richness.csv")

##############

planet_slopes <- read.csv("./results/ACTUAL_RESULTS/Planet_slopes_FINAL_Type_grouped.csv")
uas_slopes <- read.csv("./results/ACTUAL_RESULTS/UAS_slopes_FINAL.csv")

planet <- planet_slopes %>%
  select(-c(starts_with("prop"), starts_with("median"))) %>%
  mutate(mean_diff_shannon = abs(mean_diff_shannon),
         mean_diff_turnover = abs(mean_diff_turnover),
         mean_diff_richness = abs(mean_diff_richness)) %>%
  mutate(Type = factor(case_when(
    Type == "int_prox" ~ "int",
    Type == "int_cit" ~ "int",
    Type == "ext_prox" ~ "ext",
    .default = Type
  ), levels = c("int", "ext", "semi_nat"))) %>%
  filter(n_sites_shannon != 0) %>%
  select(c(predictor, Type, n_sites_shannon, mean_diff_shannon)) %>%
  mutate(predictor = fct_reorder(predictor, mean_diff_shannon,
                                 .fun = mean, na.rm = TRUE, .desc = T))

p <- planet_slopes %>%
  mutate(pred = str_remove(predictor, "B\\d{1}_")) %>%
  group_by(pred) %>%
  summarise(sum_n = sum(n_sites_shannon, n_sites_turnover, n_sites_richness))

planet_predictors <- p %>%
  mutate(sum_n = if_else(pred == "SSD", sum_n * 4, sum_n)) %>%
  arrange(desc(sum_n))


##########################################################################
uas <- uas_slopes %>%
  select(-c(starts_with("prop"), starts_with("median"))) %>%
  mutate(mean_diff_shannon = abs(mean_diff_shannon),
         mean_diff_turnover = abs(mean_diff_turnover),
         mean_diff_richness = abs(mean_diff_richness)) %>%
  select(c(predictor, agg, n_sites_shannon, mean_diff_shannon)) %>%
  mutate(predictor = fct_reorder(predictor, mean_diff_shannon,
                                 .fun = mean, na.rm = TRUE, .desc = T)) %>%
  mutate(agg = factor(case_when(
    agg == "agg1" ~ "3 cm",
    agg == "agg2" ~ "6 cm",
    agg == "agg4" ~ "12 cm"
  ), levels = c("3 cm", "6 cm", "12 cm")))


u <- uas_slopes %>%
  mutate(pred = str_remove(predictor, "B\\d{1}_")) %>%
  group_by(pred) %>%
  summarise(sum_n = sum(n_sites_shannon, n_sites_turnover, n_sites_richness))

uas_predictors <- u %>%
  mutate(sum_n = if_else(pred == "SSD", sum_n * 4, sum_n)) %>%
  arrange(desc(sum_n))

##########################################################################
combo <- bind_cols(uas_predictors, planet_predictors)
colnames(combo) <- c("UAS", "n_sites", "PlanetScope", "n_sites")
xtable(combo)
