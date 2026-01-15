library(tidyverse)
library(dplyr)

source("./helper_functions.R")

env_params <- read.table("./data/_tables/_analysisready_env_params.csv")
shannon <- read.csv("./results/ShannonH_per_site.csv")
turnover <- read.csv("./results/Species_turnover.csv")
planet_metrics <- read.csv("./results/Planet_texture_metric_results.csv")

# Shannon
shannon_slopes <- shannon %>%
  filter(month %in% c("April", "July")) %>%
  pivot_wider(
    names_from = month,
    values_from = shannon
  ) %>%
  mutate(
    shannon_slope = July - April
  ) %>%
  select(Site.No, shannon_slope)

predictor_cols <- planet_metrics %>%
  select(-date, -month, -site) %>%
  colnames()
predictor_colsI 

planet_long <- planet_metrics %>%
  filter(month %in% c("April", "July")) %>%
  pivot_longer(
    cols = all_of(predictor_cols),
    names_to = "predictor",
    values_to = "value"
  )

predictor_slopes <- planet_long %>%
  pivot_wider(
    names_from = month,
    values_from = value
  ) %>%
  mutate(
    predictor_slope = July - April
  ) %>%
  select(site, predictor, predictor_slope)
