library(tidyr)
library(dplyr)
library(ggplot2)
library(terra)

env_params <- read.table("./data/_tables/_analysisready_env_params.csv")
shannon <- read.csv("./results/ShannonH_per_site.csv")
turnover <- read.csv("./results/Species_turnover.csv")
richness <- read.csv("./results/Species_richness.csv")
planet_metrics <- read.csv("./results/Planet_texture_metric_results.csv")
uas_metrics <- read.csv("./results/UAS_texture_metric_results.csv")

# Prepare X variables ----------------------------------------------------
shannon <- shannon %>%
  mutate(siteID = Site.No) %>%
  select(-Site.No)
turnover <- turnover %>%
  mutate(siteID = site) %>%
  select(-site)

richness <- richness %>%
  mutate(richness = species_on_run) %>%
  select(-species_on_run)

s <- richness %>%
  left_join(shannon, by = c("month", "siteID")) %>%
  left_join(turnover,by = c("month", "siteID")) %>%
  mutate(site = paste0("site", siteID)) %>%
  select(-siteID)

# Prepare Y variables ----------------------------------------------------

predictor_cols <- uas_metrics %>%
  select(-date, -month, -site, -agg) %>%
  colnames()

uas_long <- uas_metrics %>%
  filter(month %in% c("April", "July")) %>%
  pivot_longer(
    cols = all_of(predictor_cols),
    names_to = "predictor",
    values_to = "value"
  )

predictor_slopes <- uas_long %>%
  group_by(site, predictor, month, agg) %>%
  summarise(
    # without summarise we get 2 columns and all July slopes will be NA
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
  select(site, predictor, predictor_slope, agg)

# sanity check
apr <- uas_long$value[uas_long$site == "site10" & uas_long$month == "April" & uas_long$predictor == "B1_CV" & uas_long$agg == "agg1"]
jul <- uas_long$value[uas_long$site == "site10" & uas_long$month == "July" & uas_long$predictor == "B1_CV" & uas_long$agg == "agg1"]
if (jul - apr == predictor_slopes$predictor_slope[predictor_slopes$site == "site10" & predictor_slopes$predictor == "B1_CV" & predictor_slopes$agg == "agg1"]){
  print("omg it works")
} else {
  print("ahhhhhhhhhhhhhhh")
}

slope_comparison <- predictor_slopes %>%
  left_join(shannon_slopes, by = "site") %>%
  left_join(turnover_slopes, by = "site") %>%
  left_join(richness_slopes, by = "site")

slope_comparison <- slope_comparison %>%
  mutate(
    same_sign_shannon = sign(predictor_slope) == sign(shannon_slope),
    same_sign_turnover = sign(predictor_slope) == sign(turnover_slope),
    same_sign_richness = sign(predictor_slope) == sign(richness_slope)
  )

predictor_summary <- slope_comparison %>%
  group_by(predictor, agg) %>%
  summarise(
    prop_same_sign_shannon = sum(same_sign_shannon, na.rm = T) / n(),
    prop_same_sign_turnover = sum(same_sign_turnover, na.rm = T) / n(),
    prop_same_sign_richness = sum(same_sign_richness, na.rm = T) / n()
  )

slope_comparison <- slope_comparison %>%
  mutate(slope_diff_shannon = case_when(
    same_sign_shannon == TRUE ~ shannon_slope - predictor_slope,
    same_sign_shannon == FALSE ~ NA
  )) %>%
  mutate(slope_diff_turnover = case_when(
    same_sign_turnover == TRUE ~ turnover_slope - predictor_slope,
    same_sign_turnover == FALSE ~ NA
  )) %>%
  mutate(slope_diff_richness = case_when(
    same_sign_richness == TRUE ~ richness_slope - predictor_slope,
    same_sign_richness == FALSE ~ NA
  ))

# then summarise
predictor_diff_summary <- slope_comparison %>%
  group_by(predictor, agg) %>%
  summarise(
    n_sites_shannon = sum(!is.na(slope_diff_shannon)),
    mean_diff_shannon = mean(slope_diff_shannon, na.rm = TRUE),
    median_diff_shannon = median(slope_diff_shannon, na.rm = TRUE),
    
    n_sites_turnover = sum(!is.na(slope_diff_turnover)),
    mean_diff_turnover = mean(slope_diff_turnover, na.rm = TRUE),
    median_diff_turnover = median(slope_diff_turnover, na.rm = TRUE),
    
    n_sites_richness = sum(!is.na(slope_diff_richness)),
    mean_diff_richness = mean(slope_diff_richness, na.rm = TRUE),
    median_diff_richness = median(slope_diff_richness, na.rm = TRUE),
  )

result <- predictor_summary %>%
  left_join(predictor_diff_summary, by = c("predictor", "agg"))

#write.csv(result, file="./results/UAS_slopes_FINAL.csv", row.names = F)