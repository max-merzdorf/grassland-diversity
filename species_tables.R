# Add "species abundance" and "species turnover" columns
# Species turnover: Delta of unique species (how different is the species composition from apr -> may)
library(tidyverse)
library(vegan)

env_params <- read.table("./data/_tables/_analysisready_env_params.csv")
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
#write.csv(shannon, file = "./results/ShannonH_per_site.csv", row.names = F)


# add species turnover -> per site approach
# with vectors for df:
t <- c()
mon <- c()
site <- c()
for (i in sites){
  species <- allspecies[allspecies$Site.No == i,]
  species <- species %>%
    mutate(turnover_time = case_when(
      month == "April" ~ 4,
      month == "July" ~ 7
    ))
  
  # all occurring species in two months:
  occ_spec <- unique(species$Species)
  occ_1 <- unique(species$Species[species$month == "April"])
  occ_2 <- unique(species$Species[species$month == "July"])
  lost <- as.numeric(summary(occ_1 %in% occ_2)["FALSE"][[1]]) # number of FALSE: species lost
  gained <- as.numeric(summary(occ_2 %in% occ_1)["FALSE"][[1]]) # number of FALSE: species gained
  
  t1 <- 1
  t2 <- (gained + lost) / length(occ_spec)
  writeLines(paste0("Site",i, "\n",
    " Turnover April: ", t1, "\n",
    " Turnover July: ", t2))
  t <- c(t, t1, t2)
  mon <- c(mon, "April", "July")
  site <- c(site, i, i)
}

df <- data.frame(site = site, month = mon, turnover = t)
#write.csv(df, "./results/Species_turnover.csv", row.names = F)

# codyn turnover test
library(codyn)
c <- collins08 %>%
  filter(year %in% c(1984, 1985, 1986))
turnover(c, time.var = "year", species.var = "species", abundance.var = "abundance", replicate.var = "replicate")
# turnover 1985 annually burned: 0.2372881

c <- c %>%
  filter(!year %in% c(1986)) %>%
  filter(replicate %in% "annually burned")

occ1 <- c %>%
  filter(year == 1984) %>%
  select(species) %>%
  unique()

occ2 <- c %>%
  filter(year == 1985) %>%
  select(species) %>%
  unique()

lost <- as.numeric(summary(occ1$species %in% occ2$species)["FALSE"][[1]]) # number of FALSE: species lost
gained <- as.numeric(summary(occ2$species %in% occ1$species)["FALSE"][[1]]) # number of FALSE: species gained

t2 <- (gained + lost) / length(unique(c$species))

# species richness table -------------------------------------------------
sprich <- subset(e, month == "April" | month == "July")
sprich <- sprich %>%
  select(siteID, Type, species_on_run, month)

write.csv(sprich, "./results/Species_richness.csv", row.names = F)

# 4 observation slope tables ---------------------------------------------

