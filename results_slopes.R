library(tidyr)
library(dplyr)
library(ggplot2)
library(terra)

source("./helper_functions.R")

env_params <- read.table("./data/_tables/_analysisready_env_params.csv")
shannon <- read.csv("./results/ShannonH_per_site.csv")
turnover <- read.csv("./results/Species_turnover.csv")
richness <- read.csv("./results/Species_richness.csv")
planet_metrics <- read.csv("./results/Planet_texture_metric_results.csv")
uas_metrics <- read.csv("./results/UAS_texture_metric_results.csv")

###### FOR SPECIES y-VARIABLES ###########################################

# Shannon slopes ---------------------------------------------------------
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

# sanity check (for the script, not me)
jul <- shannon$shannon[shannon$Site.No == 1 & shannon$month == "July"]
apr <- shannon$shannon[shannon$Site.No == 1 & shannon$month == "April"]
if(jul -  apr == shannon_slopes$shannon_slope[shannon_slopes$Site.No == 1]){
  print("Hooray")
} else {
  print("oh god")
}
rm(apr, jul)

# Turnover slopes --------------------------------------------------------
turnover_slopes <- turnover %>%
  filter(month %in% c("April", "July")) %>%
  pivot_wider(
    names_from = month,
    values_from = turnover
  ) %>%
  mutate(
    turnover_slope = July - April
  ) %>%
  select(site, turnover_slope)
# sanity check
jul <- turnover$turnover[turnover$site == 1 & turnover$month == "July"]
apr <- turnover$turnover[turnover$site == 1 & turnover$month == "April"]
if(jul -  apr == turnover_slopes$turnover_slope[turnover_slopes$site == 1]){
  print("Hooray")
} else {
  print("oh god")
}
rm(apr, jul)

# Species richness slopes ------------------------------------------------
richness_slopes <- richness %>%
  select(siteID, month, species_on_run) %>%
  filter(month %in% c("April", "July")) %>%
  pivot_wider(
    names_from = month,
    values_from = species_on_run
  ) %>%
  mutate(
    richness_slope = July - April
  ) %>%
  select(siteID, richness_slope)

# rename site identifier colums ------------------------------------------
shannon_slopes <- shannon_slopes %>%
  mutate(site = paste0("site", Site.No)) %>%
  select(site, shannon_slope)

turnover_slopes <- turnover_slopes %>%
  mutate(site = paste0("site", site)) %>%
  select(site, turnover_slope)

richness_slopes <- richness_slopes %>%
  mutate(site = paste0("site", siteID)) %>%
  select(site, richness_slope)

##########################################################################
# Planet predictor slopes ------------------------------------------------
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
apr <- planet_long$value[planet_long$site == "site1" & planet_long$month == "April" & planet_long$predictor == "B1_CV"]
jul <- planet_long$value[planet_long$site == "site1" & planet_long$month == "July" & planet_long$predictor == "B1_CV"]
if (jul - apr == predictor_slopes$predictor_slope[predictor_slopes$site == "site1" & predictor_slopes$predictor == "B1_CV"]){
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
  group_by(predictor) %>%
  summarise(
    prop_same_sign_shannon = sum(same_sign_shannon, na.rm = TRUE) / n(),
    prop_same_sign_turnover = sum(same_sign_turnover, na.rm = TRUE) / n(),
    prop_same_sign_richness = sum(same_sign_richness, na.rm = TRUE) / n()
  )

# slope diff?
# don't consider different sign predictors -> can use abs. diff
# but if I do shannon slope - predictor slope I can say:
# predictors where slope difference has negative sign will potentially overestimate species diversity
# predictors where slope difference has positive sign will potentially underestimate species diversity

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
  group_by(predictor) %>%
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
  left_join(predictor_diff_summary, by = "predictor")

#write.csv(result, file="./results/Planet_Results_FINAL.csv", row.names = F)

##########################################################################
# UAS predictor slopes ---------------------------------------------------
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
    prop_same_sign_shannon = sum(same_sign_shannon, na.rm = TRUE) / n(),
    prop_same_sign_turnover = sum(same_sign_turnover, na.rm = TRUE) / n(),
    prop_same_sign_richness = sum(same_sign_richness, na.rm = TRUE) / n()
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

#write.csv(result, file="./results/UAS_Results_FINAL.csv", row.names = F)


##########################################################################
# 4 OBSERVATION PART #####################################################
##########################################################################




# THE SCRAPYEARD ---------------------------------------------------------

# add column for abs differences
slope_comparison <- slope_comparison %>%
  mutate(
    abs_slope_diff_shannon = if_else(
      same_sign_shannon,
      abs(shannon_slope - predictor_slope),
      NA_real_
    )
  )

# or change to include proportion of same sign
predictor_absdiff_summary <- slope_comparison %>%
  group_by(predictor) %>%
  summarise(
    n_sites = n(),
    prop_same_sign = mean(same_sign, na.rm = TRUE),
    mean_abs_diff = mean(abs_slope_diff, na.rm = TRUE)
  ) %>%
  arrange(desc(prop_same_sign), mean_abs_diff)
