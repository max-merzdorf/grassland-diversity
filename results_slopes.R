library(tidyverse)
library(dplyr)
library(ggplot2)

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

# sanity check (for the script not me)
jul <- shannon$shannon[shannon$Site.No == 1 & shannon$month == "July"]
apr <- shannon$shannon[shannon$Site.No == 1 & shannon$month == "April"]
if(jul -  apr == shannon_slopes$shannon_slope[shannon_slopes$Site.No == 1]){
  print("Hooray")
} else {
  print("oh god")
}
rm(apr, jun)


predictor_cols <- planet_metrics %>%
  select(-date, -month, -site) %>%
  colnames()
predictor_cols

planet_long <- planet_metrics %>%
  filter(month %in% c("April", "July")) %>%
  pivot_longer(
    cols = all_of(predictor_cols),
    names_to = "predictor",
    values_to = "value"
  )

predictor_slopes <- planet_long %>%
  group_by(site, predictor, month) %>%
  summarise(
    # without summareise we get 2 columns and all July slopes will be NA
    value = mean(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = month,
    values_from = value
  ) %>%
  mutate(
    predictor_slope = July - April
  ) %>%
  select(site, predictor, predictor_slope)

# sanity check
apr <- planet_long$value[planet_long$site == "site1" & planet_long$month == "April" & planet_long$predictor == "B1_cv"]
jul <- planet_long$value[planet_long$site == "site1" & planet_long$month == "July" & planet_long$predictor == "B1_cv"]
if (jul - apr == predictor_slopes$predictor_slope[predictor_slopes$site == "site1" & predictor_slopes$predictor == "B1_cv"]){
  print("omg it works")
} else {
  print("ahhhhhhhhhhhhhhh")
}

# add site to every sitenumber because im stupid and didnt think about naming conventions
shannon_slopes <- shannon_slopes %>%
  mutate(site = paste0("site", Site.No)) %>%
  select(site, shannon_slope)

slope_comparison <- predictor_slopes %>%
  left_join(shannon_slopes, by = "site")

# check if sign is the same
slope_comparison <- slope_comparison %>%
  mutate(
    same_sign = sign(predictor_slope) == sign(shannon_slope)
  )

predictor_summary <- slope_comparison %>%
  group_by(predictor) %>%
  summarise(
    n_sites = n(),
    n_same_sign = sum(same_sign, na.rm = TRUE),
    prop_same_sign = n_same_sign / n_sites
  ) %>%
  arrange(desc(prop_same_sign))

# visual:
ggplot(
  slope_comparison %>% filter(predictor == "B3_glcm_contrast_std"),
  aes(predictor_slope, shannon_slope)
) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Slope comparison (April → July)")
