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

# remove sites with tree cover
planet_metrics <- planet_metrics %>%
  filter(!site %in% c("site16", "site17", "site18", "site19", "site20",
                      "site24", "site25", "site26", "site27"))

###### 2 OBSERVARTIONS PART ##############################################
# rename columns to match richness
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

# Pearson for Planet species vars ----------------------------------------
s_planet <- s %>%
  filter(!site %in% c("site16", "site17", "site18", "site19", "site20",
                      "site24", "site25", "site26", "site27")) %>%
  left_join(planet_metrics, by = c("site", "month"))

ivs <- c("richness", "shannon", "turnover")
dvs <- s_planet %>%
  select(-c(date, site, month, Type)) %>%
  names %>%
  setdiff(ivs)

# ungrouped
planet_long <- expand_grid(iv = ivs, dv = dvs) %>%
  rowwise() %>%
  mutate(
    pearson_r = cor(s_planet[[iv]], s_planet[[dv]],
                    use = "pairwise.complete.obs",
                    method = "pearson"),
    test = list(cor.test(s_planet[[iv]], s_planet[[dv]], method = "pearson")),
    p_value = test$p.value
  ) %>%
  select(-test) %>%
  ungroup()

# group by Type?
planet_long <- s_planet %>%
  pivot_longer(
    cols = all_of(dvs),
    names_to = "dependent_var",
    values_to = "dependent_value"
  )

planet_cor_results <- planet_long %>%
  pivot_longer(
    cols = all_of(ivs),
    names_to = "independent_var",
    values_to = "independent_value"
  ) %>%
  group_by(Type, independent_var, dependent_var) %>%
  summarise(
    test = list(
      cor.test(independent_value, dependent_value, method = "pearson")
    ),
    .groups = "drop"
  ) %>%
  mutate(
    pearson_r = sapply(test, \(x) x$estimate),
    p_value   = sapply(test, \(x) x$p.value),
    n         = sapply(test, \(x) x$parameter + 2)
  ) %>%
  select(-test)

#write.csv(planet_cor_results, "./results/Planet_species_vars_pearson.csv", row.names = F)

# Pearson for UAS species vars -------------------------------------------
uas_metrics <- uas_metrics %>%
  filter(month %in% c("April", "July")) # reduce months in UAS texture metrics

s_uas <- s %>%
  filter(site %in% c("site8", "site10", "site14")) %>% # reduce months in species variables to avoid NAs
  left_join(uas_metrics, by = c("site", "month"))

uas_predictor_names <- uas_metrics %>%
  select(-c(date, site, month, agg)) %>% 
  colnames()

ivs <- c("richness", "shannon", "turnover")
dvs <- s_uas %>%
  select(-c(date, site, month, Type, agg)) %>%
  names %>%
  setdiff(ivs)

# group by agg?
uas_long <- s_uas %>%
  pivot_longer(
    cols = all_of(dvs),
    names_to = "dependent_var",
    values_to = "dependent_value"
  )

uas_cor_results <- uas_long %>%
  pivot_longer(
    cols = all_of(ivs),
    names_to = "independent_var",
    values_to = "independent_value"
  ) %>%
  group_by(agg, independent_var, dependent_var) %>%
  summarise(
    test = list(
      cor.test(independent_value, dependent_value, method = "pearson")
    ),
    .groups = "drop"
  ) %>%
  mutate(
    pearson_r = sapply(test, \(x) x$estimate),
    p_value   = sapply(test, \(x) x$p.value),
    n         = sapply(test, \(x) x$parameter + 2)
  ) %>%
  select(-test)

#write.csv(uas_cor_results, "./results/UAS_species_vars_pearson.csv", row.names = F)

# Species slopes ---------------------------------------------------------
shannon_slopes <- shannon %>%
  filter(month %in% c("April", "July")) %>%
  pivot_wider(
    names_from = month,
    values_from = shannon
  ) %>%
  mutate(
    shannon_slope = July - April
  ) %>%
  select(siteID, shannon_slope)

# sanity check (for the script, not me)
jul <- shannon$shannon[shannon$siteID == 1 & shannon$month == "July"]
apr <- shannon$shannon[shannon$siteID == 1 & shannon$month == "April"]
if(jul -  apr == shannon_slopes$shannon_slope[shannon_slopes$siteID == 1]){
  print("Hooray")
} else {
  print("oh god")
}
rm(apr, jul)

turnover_slopes <- turnover %>%
  filter(month %in% c("April", "July")) %>%
  pivot_wider(
    names_from = month,
    values_from = turnover
  ) %>%
  mutate(
    turnover_slope = July - April
  ) %>%
  select(siteID, turnover_slope)
# sanity check
jul <- turnover$turnover[turnover$siteID == 1 & turnover$month == "July"]
apr <- turnover$turnover[turnover$siteID == 1 & turnover$month == "April"]
if(jul -  apr == turnover_slopes$turnover_slope[turnover_slopes$siteID == 1]){
  print("Hooray")
} else {
  print("oh god")
}
rm(apr, jul)

richness_slopes <- richness %>%
  select(siteID, month, richness) %>%
  filter(month %in% c("April", "July")) %>%
  pivot_wider(
    names_from = month,
    values_from = richness
  ) %>%
  mutate(
    richness_slope = July - April
  ) %>%
  select(siteID, richness_slope)

shannon_slopes <- shannon_slopes %>%
  mutate(site = paste0("site", siteID)) %>%
  select(site, shannon_slope)

turnover_slopes <- turnover_slopes %>%
  mutate(site = paste0("site", siteID)) %>%
  select(site, turnover_slope)

richness_slopes <- richness_slopes %>%
  mutate(site = paste0("site", siteID)) %>%
  select(site, richness_slope)

# SLOPE COMPARISONS ######################################################
# Planet predictor slopes ------------------------------------------------
predictor_cols <- planet_metrics %>%
  select(-date, -month, -site) %>%
  colnames()
predictor_cols

# add type back to planet_metrics
mgmt_type <- env_params %>%
  filter(Col_run == 1) %>%
  select(c(siteID, Type)) %>%
  mutate(site = paste0("site", siteID)) %>%
  select(-siteID) %>%
  mutate(Type = factor(case_when(
    Type == "int_prox" ~ "int",
    Type == "int_cit" ~ "int",
    Type == "ext_prox" ~ "ext",
    Type == "ext_cit" ~ "ext",
    .default = Type
  ), levels = c("int", "ext", "semi_nat")))

p <- planet_metrics %>%
  left_join(mgmt_type, by = "site")

planet_long <- p %>%
  filter(month %in% c("April", "July")) %>%
  pivot_longer(
    cols = all_of(predictor_cols),
    names_to = "predictor",
    values_to = "value"
  )

predictor_slopes <- planet_long %>%
  group_by(site, predictor, month, Type) %>%
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
  select(site, predictor, predictor_slope, Type)

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
  group_by(predictor, Type) %>%
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
  group_by(predictor, Type) %>%
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
  left_join(predictor_diff_summary, by = c("predictor", "Type"))

#write.csv(result, file="./results/Planet_slopes_FINAL.csv", row.names = F)
#write.csv(result, file="./results/ACTUAL_RESULTS/Planet_slopes_FINAL_Type_grouped.csv", row.names = F)



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

#write.csv(result, file="./results/ACTUAL_RESULTS/UAS_slopes_FINAL.csv", row.names = F)

# 4 OBSERVATION PART #####################################################

#> As we have 4 observations per site now for both the y-variables
#> veg. height [cm], % moss, % litter, ... as well as for the x-variables
#> (by selecting all months instead of only April and July) we can now use
#> the Pearson correlation to assess both strength and direction of the
#> correlation

# Pearson for Planet structure vars --------------------------------------
e <- env_params %>%
  mutate(month = case_when(
    Col_run == 1 ~ "April",
    Col_run == 2 ~ "June",
    Col_run == 3 ~ "July",
    Col_run == 4 ~ "August",
    Col_run == 5 ~ "September",
  )) %>%
  filter(month %in% c("April", "July", "August")) %>%
  mutate(site = paste0("site", siteID)) %>% # same colname as planet_metrics for grouping -> can drop siteID
  select(-c(Col_run, all_vegetation, note, Sum.all, species_on_run, siteID, Sum.vegetation)) %>%
  mutate(Type = factor(case_when(
    Type == "int_prox" ~ "int",
    Type == "int_cit" ~ "int",
    Type == "ext_prox" ~ "ext",
    Type == "ext_cit" ~ "ext",
    .default = Type
  ), levels = c("int", "ext", "semi_nat")))

e_planet <- e %>%
  filter(!site %in% c("site16", "site17", "site18", "site19", "site20",
                      "site24", "site25", "site26", "site27")) %>%
  left_join(planet_metrics, by = c("site", "month"))

ivs <- c("veg_height_cm", "grasses", "herbs", "moss", "litter", "bare.soil")
dvs <- e_planet %>%
  select(-c(date, site, month, Type)) %>%
  names %>%
  setdiff(ivs)

# group by Type
planet_long <- e_planet %>%
  pivot_longer(
    cols = all_of(dvs),
    names_to = "dependent_var",
    values_to = "dependent_value"
  )

planet_cor_results <- planet_long %>%
  pivot_longer(
    cols = all_of(ivs),
    names_to = "independent_var",
    values_to = "independent_value"
  ) %>%
  group_by(Type, independent_var, dependent_var) %>%
  summarise(
    test = list(
      cor.test(independent_value, dependent_value, method = "pearson")
    ),
    .groups = "drop"
  ) %>%
  mutate(
    pearson_r = sapply(test, \(x) x$estimate),
    p_value   = sapply(test, \(x) x$p.value),
    n         = sapply(test, \(x) x$parameter + 2)
  ) %>%
  select(-test)

#write.csv(planet_cor_results, "./results/ACTUAL_RESULTS/Planet_structure_vars_pearson.csv", row.names = F)

# ungrouped
cor_long <- expand_grid(iv = ivs, dv = dvs) %>%
  rowwise() %>%
  mutate(
    pearson_r = cor(e[[iv]], e[[dv]],
                    use = "pairwise.complete.obs",
                    method = "pearson")
  ) %>%
  ungroup()

cor_long <- cor_long %>%
  rowwise() %>%
  mutate(
    test = list(cor.test(e[[iv]], e[[dv]], method = "pearson")),
    p_value = test$p.value
  ) %>%
  select(-test) %>%
  ungroup()

# Pearson for UAS structure vars -----------------------------------------
uas_metrics <- read.csv("./results/UAS_texture_metric_results.csv")
env_params <- read.table("./data/_tables/_analysisready_env_params.csv")

# redo e because I left joined planet_metrics earlier
e <- env_params %>%
  mutate(month = case_when(
    Col_run == 1 ~ "April",
    Col_run == 2 ~ "June",
    Col_run == 3 ~ "July",
    Col_run == 4 ~ "August",
    Col_run == 5 ~ "September",
  )) %>%
  filter(month %in% c("April", "July", "August")) %>%
  mutate(site = paste0("site", siteID)) %>% # same colname as planet_metrics for grouping -> can drop siteID
  select(-c(Col_run, all_vegetation, note, Sum.all, species_on_run, siteID, Sum.vegetation))

uas_metrics <- uas_metrics %>%
  filter(month %in% c("April", "July", "August")) # reduce months

e_uas <- e %>%
  filter(site %in% c("site8", "site10", "site14")) %>%
  left_join(uas_metrics, by = c("site", "month"))

uas_predictor_names <- uas_metrics %>%
  select(-c(date, site, month, agg)) %>% 
  colnames()

ivs <- c("veg_height_cm", "grasses", "herbs", "moss", "litter", "bare.soil")
dvs <- e_uas %>%
  select(-c(date, site, month, Type, agg)) %>%
  names %>%
  setdiff(ivs)

# group by agg?
uas_long <- e_uas %>%
  pivot_longer(
    cols = all_of(dvs),
    names_to = "dependent_var",
    values_to = "dependent_value"
  )

uas_cor_results <- uas_long %>%
  pivot_longer(
    cols = all_of(ivs),
    names_to = "independent_var",
    values_to = "independent_value"
  ) %>%
  group_by(agg, independent_var, dependent_var) %>%
  summarise(
    test = list(
      cor.test(independent_value, dependent_value, method = "pearson")
    ),
    .groups = "drop"
  ) %>%
  mutate(
    pearson_r = sapply(test, \(x) x$estimate),
    p_value   = sapply(test, \(x) x$p.value),
    n         = sapply(test, \(x) x$parameter + 2)
  ) %>%
  select(-test)

#write.csv(uas_cor_results, "./results/UAS_structure_vars_pearson.csv", row.names = F)

# previous approach
euas <- env_params %>%
  mutate(month = case_when(
    Col_run == 1 ~ "April",
    Col_run == 2 ~ "June",
    Col_run == 3 ~ "July",
    Col_run == 4 ~ "August",
    Col_run == 5 ~ "September",
  )) %>%
  filter(month %in% c("April", "July", "August")) %>%
  filter(siteID %in% c(8, 10, 14)) %>%
  mutate(site = paste0("site", siteID)) %>% # same colname as planet_metrics for grouping -> can drop siteID
  select(-c(Col_run, all_vegetation, note, Sum.all, species_on_run, siteID, Sum.vegetation)) %>%
  left_join(uas_metrics, by = c("site", "month"))

ivs <- c("veg_height_cm", "grasses", "herbs", "moss", "litter", "bare.soil")
dvs <- euas %>%
  select(-c(date, site, month, Type)) %>%
  names %>%
  setdiff(ivs)

uas_cor_long <- expand_grid(iv = ivs, dv = dvs) %>%
  rowwise() %>%
  mutate(
    pearson_r = cor(euas[[iv]], euas[[dv]],
                    use = "pairwise.complete.obs",
                    method = "pearson"),
    test = list(cor.test(euas[[iv]], euas[[dv]], method = "pearson")),
    p_value = test$p.value
  ) %>%
  select(-test) %>%
  ungroup()


# THE SCRAPYARD ----------------------------------------------------------

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
