# Add "species abundance" and "species turnover" columns
# Species turnover: Delta of unique species (how different is the species composition from apr -> may)
library(tidyverse)
library(vegan)

env_params <- read.table("./data/_tables/_analysisready_env_params.csv")
plots <- sf::st_read("./data/_vector/_analysisready_plots.gpkg")
allspecies <- read.table("./data/_tables/_analysisready_allspecies.csv")

# add date colum to env_params
e <- env_params %>%
  mutate(month = case_when(
    Col_run == 1 ~ "April",
    Col_run == 2 ~ "June",
    Col_run == 3 ~ "July",
    Col_run == 4 ~ "August",
    Col_run == 5 ~ "September",
  ))

# add species abundance to allspecies (decimal londo scale)
unique(allspecies$Coverage_londo)
#> "*1" "1" "*2" "2" "3" "5" "*4" "4" "7" "int_prox"
#> "int_prox" is a error line, remove:
allspecies <- allspecies[allspecies$Coverage_londo != "int_prox",]

# arithmetic mean of upper and lower limits after Dembicz / Dengler 2025
allspecies <- allspecies %>%
  mutate(londo_decimal = case_when(
    Coverage_londo == "*1" ~ 0.5,
    Coverage_londo == "*2" ~ 2,
    Coverage_londo == "*4" ~ 4,
    Coverage_londo == "1" ~ 10,
    Coverage_londo == "2" ~ 20,
    Coverage_londo == "3" ~ 30,
    Coverage_londo == "4" ~ 40,
    Coverage_londo == "5" ~ 50,
    Coverage_londo == "6" ~ 60,
    Coverage_londo == "7" ~ 70,
    Coverage_londo == "8" ~ 80,
    Coverage_londo == "9" ~ 90,
    Coverage_londo == "10" ~ 97.5,
  ))

allspecies <- allspecies %>%
  mutate(month = case_when(
    Collection_Run == 1 ~ "April",
    Collection_Run == 2 ~ "June",
    Collection_Run == 3 ~ "July",
    Collection_Run == 4 ~ "August",
    Collection_Run == 5 ~ "September",
  ))

# calculate shannons H' per site per date
sites <- unique(allspecies$Site.No)
months <- unique(allspecies$month)

shannon <- allspecies %>%
  group_by(Site.No, month) %>%
  summarise(shannon = vegan::diversity(londo_decimal, index = "shannon"), .groups = "drop")
#write.csv(shannon, file = "./ShannonH_per_site.csv")

# add species turnover
turnover <- allspecies %>%
  filter(month %in% c("April", "July")) %>%
  group_by(Site.No, month) %>%
  summarise(Species = list(unique(Species)), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = month, values_from = Species) %>%
  mutate(
    gained = lengths(setdiff(July, April)),
    lost   = lengths(setdiff(April, July)),
    turnover = gained - lost
  ) %>%
  select(Site.No, turnover)

write.csv(turnover, "./results/Species_turnover.csv")
