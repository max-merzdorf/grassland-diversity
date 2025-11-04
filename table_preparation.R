# This script takes field observation data from the Hubland meadows project and
# brings it into the right format and cleans the data

library(GLCMTextures)
library(terra)
library(sf)
library(dplyr)
library(stringr)
library(ggplot2)
library(gridExtra)
source("./uav_multispectral_indices.R")

################################################################################
##### PREPARE TABLES ###########################################################

# table with all recorded species across all sites and all runs
allspecies <- read.table("./data/_tables/All_runs_veg_londo.csv", sep=";", dec=".", header=T)
# preallocate vectors
species_total <- rep(NA, 39)
species_run1 <- rep(NA, 39)
species_run3 <- rep(NA, 39)
siteids <- seq(1,39)
spdf <- as.data.frame(cbind(siteID = siteids, species_run1, species_run3, species_total))
# how many species recorded per site per run?
for (i in 1:39) {
  sitedf <- allspecies[allspecies$Site.No == i,]
  run1 <- sitedf[sitedf$Collection_Run == 1,]
  run3 <- sitedf[sitedf$Collection_Run == 3,]
  
  species_total[i] <- length(unique(sitedf$Species))
  species_run1[i] <- length(unique(run1$Species))
  species_run3[i] <- length(unique(run3$Species))
}
spdf$species_run1 <- species_run1
spdf$species_run3 <- species_run3
spdf$species_total <- species_total
rm(run1, run3, sitedf, i, siteids)

# table with 5 runs for each of the 39 sites
env_params <- read.table("./data/_tables/env_parameters_.csv", sep=";", dec=".", header = T)

# add column for species recorded per run (only for runs 1 and 3, rest are NA)
species_on_run <- c(species_run1, rep(NA, 39), species_run3, rep(NA, 39*2))
env_params$species_on_run <- species_on_run

# rename "site" to "siteID" for matching
colnames(env_params)[colnames(env_params) == "site"] <- "siteID"

# plots has "Nummer" as ID column
subplots <- st_read("./data/_vector/subplots_rectangles.gpkg")
superplots <- st_read(dsn = "./data/_vector/sampling_sites_large_int_ext_seminat.gpkg")
superplots <- st_transform(superplots, st_crs(x = subplots))
plots <- st_join(subplots, y = superplots, join = st_intersects)
plots <- st_transform(plots, crs=32632)
rm(subplots)


# remove unnecessary columns:
#plots <- subset(x = plots, select = c(-Date, -Note, -Photo))
# rename "No" to "siteID" as a matching column for merging later:
colnames(plots)[colnames(plots) == "Nummer"] <- "siteID"

# merge plots and spdf data frames on the ID column
plots <- merge(plots, spdf, by="siteID", all.x=T)

# convert characters to numeric and replace , with .
# -> in plots:
plots$siteID <- as.numeric(plots$siteID)

# -> in env_params:
for (i in 4:12){
  env_params[,i] <- gsub(",",".",env_params[,i])
  env_params[,i] <- as.numeric(env_params[,i])
}
class(env_params[,5])


# add management type:
plots <- merge(plots, env_params[1:39, c("siteID", "Type")], by="siteID", all.x=T)

# some values for each run:
# already written to file so commented out!
#for (i in 1:5){
#  
#  runparams <- env_params[env_params$Col_run == i,][,5:10]
#  colnames(runparams) <- paste0("run", i, "_", colnames(env_params[env_params$Col_run == i,][,5:10]))
#  
#  plots <- cbind(plots, runparams)
#}

# redued df for UAV sites:
# min max mean sd of vegetation height (& maybe others):
veg_height_mean <- c()
veg_height_sd <- c()
veg_height_min <- c()
veg_height_max <- c()

for (i in 1:nrow(plots)){
  veg_height_mean <- c(veg_height_mean, mean(env_params$veg_height_cm[env_params$site == i]))
  veg_height_sd <- c(veg_height_sd, sd(env_params$veg_height_cm[env_params$site == i]))
  veg_height_min <- c(veg_height_min, min(env_params$veg_height_cm[env_params$site == i]))
  veg_height_max <- c(veg_height_max, max(env_params$veg_height_cm[env_params$site == i]))
}

# i can just cbind because plots is sorted the same way the vectors are
plots <- cbind(plots, veg_height_max, veg_height_min, veg_height_mean, veg_height_sd)
rm(i, veg_height_max, veg_height_mean, veg_height_min, veg_height_sd, species_run1,
   species_run3, species_total)

# write to disk (plots as sf)
#sf::st_write(plots, dsn = "./data/_vector/_analysisready_plots.gpkg")
