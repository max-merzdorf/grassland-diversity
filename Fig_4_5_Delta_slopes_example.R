library(dplyr)
library(tidyverse)
library(xtable)
library(ggplot2)
library(terra)
library(sf)
# First: cread all the result files and check
uas_species_vars <- read.csv("./results/ACTUAL_RESULTS/UAS_species_vars_pearson.csv")
uas_structure_vars <- read.csv("./results/ACTUAL_RESULTS/UAS_structure_vars_pearson.csv")
uas_predictors <- read.csv("./results/UAS_texture_metric_results.csv")

planet_slopes <- read.csv("./results/ACTUAL_RESULTS/Planet_slopes_FINAL.csv")
planet_species_vars <- read.csv("./results/ACTUAL_RESULTS/Planet_species_vars_pearson.csv")
planet_structure_vars <- read.csv("./results/ACTUAL_RESULTS/Planet_structure_vars_pearson.csv")
planet_predictors <- read.csv("./results/Planet_texture_metric_results.csv")

uas_latex <- uas_slopes %>%
  select(-c(n_sites_shannon, n_sites_turnover, n_sites_richness))


##### 3. BAND REFLECTANCE VALUES ######
planet <- rast("./data/_raster/planet/Planet_8bit.tif")
sf_use_s2(FALSE) # turn off spherical geometry use
plots <- sf::st_read("./data/_vector/sampling_sites_large_int_ext_seminat.gpkg") %>%
  st_transform("EPSG:32632") %>%
  st_buffer(dist = 30) %>%
  vect()

p <- planet %>%
  crop(plots[plots$Nummer == 10]) %>%
  mask(plots[plots$Nummer == 10]) %>%
  values()
ggplot()

##### 4.5 Rate of change example #####
ex <- s_planet %>% # (from results_slopes.R)
  select(shannon, B3_RaoQ_w3, B4_RaoQ_w3, site, date) %>%
  filter(site == "site1")
exl <- pivot_longer(ex, cols=-c(site, date))
p <- ggplot(exl, aes(x = date, y = value, col = name)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = F) +
  scale_colour_viridis_d(option = "E") +
  xlab("Date [numeric]") +
  ylab("Value") +
  guides(col = guide_legend(title = "Variable")) +
  theme_light()
p
ggsave("./images/graphs/section_4_5_Rate_of_change_example.png", p,
       height = 1000, width = 2000, units = "px",dpi = 300)

##### 5.1.1 UAS slope deltas #####


# xtable:
uxvalues <- u %>%
  select(predictor, agg, starts_with("mean_diff"))

xtable(uxvalues[1:5,])

uxranks <- u %>%
  mutate(prop_ssS = prop_same_sign_shannon,
         prop_ssT = prop_same_sign_turnover,
         prop_ssR = prop_same_sign_richness) %>%
  select(-c(starts_with("prop_same"))) %>%
  mutate(xquer_dS = mean_diff_shannon,
         xquer_dT = mean_diff_turnover,
         xquer_dR = mean_diff_richness) %>%
  select(c(predictor, agg, rank_meanS, rank_meanT, rank_meanR, mean_ranksum))

xtable(uxranks[1:5,])
